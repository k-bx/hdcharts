{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lucid.Server where

import           Control.Monad
import qualified Data.ByteString.Char8 as SC
import           Happstack.Server
import Lucid

-- | happstack server utils
serve :: ServerPart Response -> IO ()
serve response =
    simpleHTTP (nullConf {port = 8001}) $ do
        decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)
        response

includeLibDir :: ServerPart Response -> ServerPart Response
includeLibDir page = msum
  [ dir "" page
  , dir "lib" $ serveDirectory EnableBrowsing [] "lib"
  ]

instance ToMessage (Html ()) where
    toContentType _ = SC.pack "text/html; charset=UTF-8"
    toMessage       = renderBS
