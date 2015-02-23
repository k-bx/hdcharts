{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

{-

a socket MVC

The addition of the websocket is an example of how a pre-existing MVC structure can be wrapped by another.

-}

module Web.Socket where

import           Control.Applicative
import           Control.Concurrent.Async as Async
import           Control.Exception (finally, bracket)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Managed
import           Data.Aeson hiding ((.=))
import           Data.Aeson.ByteString ()
import           Data.ByteString.Lazy (ByteString)
import           Data.Data
import           Data.Default
import qualified Data.Foldable as F
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Generics
import           MVC
import qualified Network.Socket as S
import qualified Network.WebSockets as WS
import qualified Pipes.Prelude as Pipes
import           Pipes.Util

data SocketConfig
    = SocketConfig
    { _cPort :: Int
    , _cHost :: String
    }

makeLenses ''SocketConfig

defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig 3566 "0.0.0.0"

instance Default SocketConfig where
    def = defaultSocketConfig

data SocketComms
    = ServerReady
    | CloseSocket
    | ServerClosed
    | ClientClosed
    | ClientReady
    | SocketLog ByteString
    deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON SocketComms
instance ToJSON SocketComms

wsSocket ::
    SocketConfig ->
    Managed (View (Either SocketComms ByteString),
             Controller (Either SocketComms ByteString))
wsSocket c = join $ managed $ \k -> do
    (oC, iC, sealC) <- spawn' Unbounded
    (oV, iV, sealV) <- spawn' Unbounded

    let handler pending =
            with (managed (withConnection pending)) $ \conn -> do
                aC <- async $ do
                    runEffect $
                        forever (do
                            d <- lift $ WS.receiveData conn
                            yield d) >->
                        Pipes.map Right >->
                        toOutput oC
                    atomically sealC
                aV <- async $ do
                    runEffect $
                        fromInput iV >->
                        until' (Left CloseSocket) >->
                        forever (do
                             x <- await
                             case x of
                                 Left CloseSocket -> do
                                      lift $ WS.sendClose conn ("closing" :: Text)
                                      lift $ putStrLn "CloseSocket!"
                                 Right stream ->
                                      lift $ WS.sendTextData conn stream
                                 _ -> return ())
                    atomically sealV
                link aV
                link aC
                _ <- waitAnyCancel [aV, aC]
                void $ atomically $ send oC (Left ServerClosed)

    aServer <- async $ run (c^.cHost) (c^.cPort) handler
    (void . atomically . send oC) (Left ServerReady)

    result <- k (return
                 ( asSink (void . atomically . send oV)
                 , asInput iC
                 ))
              <* atomically sealC
              <* atomically sealV

    cancel aServer
    return result

withConnection :: WS.PendingConnection -> (WS.Connection -> IO r) -> IO r
withConnection pending =
    bracket
    (WS.acceptRequest pending)
    (\conn -> WS.sendClose conn ("closing" :: ByteString))

withSocket :: String -> Int -> (S.Socket -> IO c) -> IO c
withSocket host port = bracket (WS.makeListenSocket host port) S.sClose

runServerWith' :: String -> Int -> WS.ConnectionOptions -> WS.ServerApp -> IO ()
runServerWith' host port opts app = S.withSocketsDo $ do
    sock <- WS.makeListenSocket host port
    forever $ do
        (conn, _) <- S.accept sock
        a <- async $ Control.Exception.finally
             (runApp' conn opts app)
             (S.sClose conn)
        link a
        return ()
    S.sClose sock

runApp' :: S.Socket
       -> WS.ConnectionOptions
       -> WS.ServerApp
       -> IO ()
runApp' socket opts app = do
    pending <- WS.makePendingConnection socket opts
    app pending

run :: String -> Int -> WS.ServerApp -> IO r
run host port app = with (managed (withSocket host port)) $ \sock ->
    forever $ do
        -- TODO: top level handle
        (conn, _) <- S.accept sock
        a         <- async $ Control.Exception.finally (runApp' conn WS.defaultConnectionOptions app) (S.sClose conn)
        link a
        return ()

stdinClient :: WS.ClientApp ()
stdinClient conn = do
    putStrLn "echoClient Connected!"
    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ Text.putStrLn msg
        -- Read from stdin and write to WS
    let loop' = do
            line <- Text.getLine
            unless (Text.null line) $ WS.sendTextData conn line >> loop'
    loop'
    WS.sendClose conn ("Bye!" :: Text)

echoClient :: WS.ClientApp ()
echoClient conn = do
    putStrLn "."
    -- Fork a thread that writes WS data to stdout
    a <- async $ forever $ do
        WS.sendTextData conn (encode [99::Int])
        msg <- WS.receiveData conn :: IO ByteString
        putStrLn ".1"
        WS.sendTextData conn msg
        putStrLn $ ".2:" <> show msg
    link a


runClient :: SocketConfig -> IO ()
runClient c = WS.runClient (c^.cHost) (c^.cPort) "/" echoClient

wsEchoClient ::
    SocketConfig ->
    Managed (View SocketComms, Controller SocketComms)
wsEchoClient c = join $ managed $ \k -> do
    ref <- newIORef Nothing
    (oV, iV, sealV) <- spawn' Unbounded
    (oC, iC, sealC) <- spawn' Unbounded
    aV <- async $ do
        runEffect $
            fromInput iV >->
            Pipes.chain (\x -> case x of
                ServerReady -> do
                    aClient <- async $ WS.runClient (c^.cHost) (c^.cPort) "/" echoClient
                    writeIORef ref (Just aClient)
                    (void . atomically . send oC) ClientReady
                ServerClosed -> do
                    F.mapM_ cancel =<< readIORef ref
                    (void . atomically . send oC) ClientClosed
                _ -> return ()) >->
            forever await
        atomically sealV
        atomically sealC

    res <- k (return
       ( asSink (void . atomically . send oV)
       , asInput iC))
        <* atomically sealV
        <* atomically sealC
    cancel aV
    return res
