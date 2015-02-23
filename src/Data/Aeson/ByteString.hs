{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.ByteString where

import           Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Control.Applicative

instance ToJSON B.ByteString where
    toJSON = String . Text.decodeUtf8
    {-# INLINE toJSON #-}

instance FromJSON B.ByteString where
    parseJSON (String t) = pure . Text.encodeUtf8 $ t
    parseJSON v          = typeMismatch "ByteString" v
    {-# INLINE parseJSON #-}

instance ToJSON L.ByteString where
    toJSON = toJSON . strict
    {-# INLINE toJSON #-}

instance FromJSON L.ByteString where
    parseJSON (String t) = pure . lazy $ t
    parseJSON v          = typeMismatch "Lazy ByteString" v
    {-# INLINE parseJSON #-}

strict :: L.ByteString -> Text
strict = Text.decodeUtf8 . B.concat . L.toChunks
{-# INLINE strict #-}

lazy :: Text -> L.ByteString
lazy = L.fromChunks . (:[]) . Text.encodeUtf8
{-# INLINE lazy #-}
