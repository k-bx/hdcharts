{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- http://localhost:8001/
-- type 'q' in terminal to quit server

module Web.Play.MVC where

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad
import           Control.Monad.State.Strict (State, put, get)
import           Data.Aeson hiding ((.=))
import           Data.Aeson.ByteString ()
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Data
import           Data.Maybe
import           GHC.Generics
import           Happstack.Server
import           Lucid
import           Lucid.Server
import           MVC
import qualified MVC.Prelude as MVC
import           Pipes.Internal (unsafeHoist)
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Util as Pipes
import qualified Pipes.Monoid as Pipes
import           Web.Page
import           Web.Play.Types
import           Web.Socket

data In a
    = Stream a
    | Echo a
    | PlayCommand PlayCommand
    | StateChange PlayState
    | SocketComms SocketComms
    | Log String
    | ServerQuit
    deriving (Show, Read, Eq, Data, Typeable, Generic)

instance (FromJSON a) => FromJSON (In a)
instance (ToJSON a) => ToJSON (In a)

makePrisms ''In

parseIn
    :: A.Parser (In a)
parseIn
    =   A.string "go"        *> return (PlayCommand Go) 
    <|> A.string "stop"      *> return (PlayCommand Stop) 
    <|> A.string "quit"      *> return (PlayCommand Quit) 
    <|> A.string "first"     *> return (PlayCommand First) 
    <|> A.string "last"      *> return (PlayCommand Last) 
    <|> A.string "q"         *> return ServerQuit 
    <|> A.string "step "     *> ((PlayCommand . Step)     <$> A.decimal)
    <|> A.string "speed "    *> ((PlayCommand . Speed)    <$> A.double) 

parseInOrRead
    :: (Read a)
    => A.Parser (In a)
parseInOrRead
    =   parseIn
    <|> do
        res <- maybeRead . SC.unpack <$> A.takeByteString
        case res of
            Nothing -> mzero
            Just a -> return a
  where
    maybeRead :: (Read a) => String -> Maybe a
    maybeRead = fmap fst . listToMaybe . filter (Prelude.null . snd) . reads 

data Out a
    = StreamOut a
    | PlayStateOut PlayState -- change of state
    | LogOut String
    | SocketOut SocketComms
    deriving (Show, Eq, Data, Typeable, Generic)

instance (FromJSON a) => FromJSON (Out a)
instance (ToJSON a) => ToJSON (Out a)

makePrisms ''Out

-- helpers (up here because of template haskell)
sleep :: Double -> IO ()
sleep t =
  threadDelay $
  floor (t * 10 ^ 6)

stdinParsed :: (Show a) => A.Parser a -> Managed (Controller a)
stdinParsed parser = MVC.producer Single $
                Pipes.stdinLn >->
                Pipes.map (A.parseOnly parser . SC.pack) >->
                Pipes.eitherP


-- | default model
mainPipe
    :: (Show a, Eq a)
    => Pipe a b Identity ()
    -> Pipe (In a) (Out b) (State PlayState) ()
mainPipe p
    =   Pipes.until' ServerQuit
    >-> Pipes.until' (SocketComms ServerClosed)
    >-> (  handleStream p
        `Pipes.ma` handlePlayCommand
        `Pipes.ma` handleStateChange
        `Pipes.ma` handleLog)

handleStream
    :: Pipe a b Identity ()
    -> Pipe (In a) (Out b) (State PlayState) ()
handleStream p
    =   Pipes.map (preview _Stream)
    >-> Pipes.justP
    >-> unsafeHoist lift p
    >-> Pipes.map StreamOut

handlePlayCommand
    :: (Eq a)
    => Pipe (In a) (Out b) (State PlayState) ()
handlePlayCommand
    =   Pipes.map (preview _PlayCommand)
    >-> Pipes.justP
    >-> forever (do
    a <- await
    case a of
        Go -> pPlaying .= True
        Stop -> pPlaying .= False
        Quit -> yield (SocketOut CloseSocket)
        First ->
            pTargetFrame .= Just 0
        Last -> do
            end <- use pTotalFrames
            pTargetFrame .= case end of
                Nothing -> Nothing
                Just t -> Just t
            pFast .= True
        Speed t -> pSpeed .= t
        Step t -> do
            pStep .= Just t
            pFast .= True
            pPlaying .= True
    p <- lift get
    yield (PlayStateOut p))

handleStateChange
    :: Pipe (In a) (Out b) (State PlayState) ()
handleStateChange
    =   Pipes.map (preview _StateChange)
    >-> Pipes.justP
    >-> Pipes.chain put
    >-> Pipes.map PlayStateOut

handleLog
    :: (Show a, Monad m)
    => Pipe (In a) (Out b) m ()
handleLog = Pipes.map (preview _Log) >-> Pipes.justP >-> Pipes.map LogOut

producerPlay'
     :: PlayState
     -> Producer a IO ()
     -> Managed (View PlayState, Controller (In a))
producerPlay' initPlay p = join $ managed $ \k -> do
    (oV, iV, sealV) <- spawn' (Latest initPlay)
    let resetP = MVC.producer Single (producerReset iV p)
    res <- k $ (,) <$> pure (asSink (void . atomically . send oV)) <*> resetP
    atomically sealV
    return res

producerReset :: MVC.Input PlayState -> Producer a IO () -> Producer (In a) IO ()
producerReset i prod = loop' prod prod
  where
    loop' p0 p1 = do
        p <- lift $ fmap fromJust $ atomically $ recv i
        -- lift $ putStrLn $ "loop:" <> show (p^.pPlaying) <> ":"
        case p^.pTargetFrame of
            Nothing ->
                if p^.pPlaying
                then (do
                      lift $
                          unless (p^.pFast) $ sleep (p^.pSpeed)
                      p' <- lift $ fmap fromJust $ atomically $ recv i
                               -- in case its changed during sleep
                      if isNothing (p'^.pTargetFrame) && p'^.pPlaying
                      then (do
                          n <- lift $ next p1
                          case n of
                              Left _ -> return ()
                              Right (a', p1') -> do
                                  case p'^.pStep of
                                      Nothing -> yield $ StateChange (pFrame +~ 1 $ p')
                                      Just 1 -> yield $ StateChange (pFrame +~ 1 $ pStep .~ Nothing $ pFast .~ False $ pPlaying .~ False $ p')
                                      Just x' -> yield $ StateChange (pFrame +~ 1 $ pStep .~ Just (x'-1) $ p')
                                  yield (Stream a')
                                  loop' p0 p1')
                      else (do
                          lift $ sleep (p'^.pSleep)
                          loop' p0 p1))
                else (do
                      lift $ sleep (p^.pSleep)
                      loop' p0 p1)
            Just frame ->
                if | p^.pFrame == frame -> 
                         yield $ StateChange ( 
                         pTargetFrame .~ Nothing $ 
                         pRedraw .~ True $ 
                         pFast .~ False $ 
                         p)
                   | p^.pDropOk && frame < p^.pFrame -> do
                      yield $ StateChange ( 
                             pTargetFrame .~ Nothing $
                             pFrame .~ frame $ 
                             pRedraw .~ True $
                             pPlaying .~ False $ 
                             pFast .~ False $ 
                             p)
                      loop' p0 (p0 >-> Pipes.drop frame)
                   | not (p^.pDropOk) && frame < p^.pFrame -> do
                      yield $ StateChange ( 
                             pRedraw .~ False $
                             pFast .~ True $
                             pFrame .~ 0 $ 
                             p)
                      loop' p0 p0
                   | p^.pDropOk && frame > p^.pFrame -> do
                      yield $ StateChange ( 
                         pTargetFrame .~ Nothing $
                         pFrame .~ frame $ 
                         pRedraw .~ True $ 
                         pFast .~ False $ 
                         p)
                      loop' p0 (p1 >-> Pipes.drop (frame - p^.pFrame))
                   | not (p^.pDropOk) && frame > p^.pFrame -> do
                      yield $ StateChange ( 
                         pRedraw .~ False $ 
                         pFast .~ True $ 
                         p)
                      do
                          n <- lift $ next p1
                          case n of
                              Left _ -> return ()
                              Right (a', p1') ->
                                  do
                                      yield $ StateChange (pFrame +~ 1 $ p)
                                      yield (Stream a')
                                      loop' p0 p1'
                   | otherwise -> error "play"

vcPlay
    :: (FromJSON b, ToJSON a, Show b, Show a)
    => PlayState
    -> SocketConfig
    -> Producer b IO ()
    -> Producer (In b) IO ()
    -> Managed (View (Out a), Controller (In b))
vcPlay initialPlay sc prod auto
    = join $ managed $ \k ->
        k $ do
            (vStream,cStream) <- producerPlay' initialPlay prod
            (vSocket,cSocket) <- wsSocket sc
            (,) <$>
                   (handles _PlayStateOut <$>
                    pure (contramap
                          (Right . (encode :: Out Int -> C.ByteString) .
                           PlayStateOut)
                          vSocket))
                <> (handles _PlayStateOut <$>
                    pure vStream)
                -- <> pure (contramap show MVC.stdoutLines)
                <> pure (handles _SocketOut (contramap Left vSocket))
                <> (handles _StreamOut <$>
                    pure (contramap (Right . encode . StreamOut) vSocket))

                <*>
                   pure (socketToIn <$> cSocket)
                <> stdinParsed parseIn
                <> pure cStream
                <> MVC.producer Single auto

  where
    socketToIn (Left comms) = SocketComms comms
    socketToIn (Right x) = case eitherDecode x of
        Left e -> Log $ "decode failed with: " <> e
        Right x' -> x'


runPlay
    :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
    => Producer b IO ()
    -> a
    -> PlayState
    -> IO PlayState
runPlay prod page'' p = do
    aRun <- async $
        runMVC
        p
        (asPipe $ mainPipe cat)
        (vcPlay p defaultSocketConfig prod (return ()))
    aServe <- async $
        serve $ ok $ toResponse page''
    res <- wait aRun
    cancel aServe
    return res

-- wrapping the model in a new state (to enable testing, replay etc)
data SWrap o a b = SWrap
               { _wrapOrig :: o
               , _wrapOuts :: [a]
               , _wrapIns :: [b]
               } deriving (Show, Eq)

makeLenses ''SWrap

runMVCWrapped :: o -> Pipe b a (State o) () -> Managed (View a, Controller b) -> IO (SWrap o a b)
runMVCWrapped s pipe vc = do
    s' <- runMVC (SWrap s [] []) (asPipe $ wrapPipe pipe) vc
    return (wrapOuts %~ reverse $ wrapIns %~ reverse $ s')
  where
    wrapPipe :: Pipe b a (State o) () -> Pipe b a (State (SWrap o a b)) ()
    wrapPipe p = Pipes.chain (\x -> wrapIns %= (:) x)
             >-> hoist (zoom wrapOrig) p
             >-> Pipes.chain (\x -> wrapOuts %= (:) x)

runPlayWrapped
    :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
    => Producer b IO ()
    -> a
    -> PlayState
    -> IO (SWrap PlayState (Out b) (In b))
runPlayWrapped prod page'' p = do
    aRun <- async $ runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod (return ()))
    aServe <- async $
        serve $ ok $ toResponse page''
    res <- wait aRun
    cancel aServe
    return res

runPlayWrappedAuto
    :: (ToMessage a, FromJSON b, ToJSON b, Show b, Eq b)
    => Producer b IO ()
    -> Producer (In b) IO ()
    -> a
    -> PlayState
    -> IO (SWrap PlayState (Out b) (In b))
runPlayWrappedAuto prod auto page'' p = do
    aRun <- async $ runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod auto)
    aServe <- async $
        serve $ ok $ toResponse page''
    res <- wait aRun
    cancel aServe
    return res

responsePlay
    :: (FromJSON b,ToJSON b, Show b, Eq b)
    => Producer b IO ()
    -> Html ()
    -> PlayState
    -> ServerPartT IO Response
responsePlay prod page'' p = do
    _ <- liftIO $ async $
         runMVC p (asPipe $ mainPipe cat) (vcPlay defaultPlayState defaultSocketConfig prod (return ()))
    ok $ toResponse page''

-- tests
tPlayAuto
    :: Int
    -> Producer (In [Int]) IO ()
    -> IO (SWrap PlayState (Out [Int]) (In [Int]))
tPlayAuto n auto = do
    let prod = MVC.each [1..n] >-> Pipes.map (:[])
        p = pTotalFrames .~ Just n $ defaultPlayState
    runMVCWrapped p (mainPipe cat) (vcPlay p defaultSocketConfig prod auto)

tGoStop
    :: IO (SWrap PlayState (Out [Int]) (In [Int]))
tGoStop = tPlayAuto 10 $ do
    yield $ PlayCommand Go
    lift $ sleep 3.3
    yield $ PlayCommand Stop
    lift $ sleep 1.2
    yield ServerQuit

testPlay'
    :: PlayState
    -> Int
    -> IO (SWrap PlayState (Out Int) (In Int))
testPlay' p n =
    runPlayWrappedAuto
     (MVC.each [1..n])
     (return ())
     (renderPage $ play jsEcho p defaultSocketConfig)
     p

testManual :: IO (SWrap PlayState (Out Int) (In Int))
testManual = testPlay' (pTotalFrames .~ Just 1000 $ defaultPlayState) 1000



