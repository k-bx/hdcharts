{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.XY where

import           Data.Aeson
import           Data.Data
import qualified Data.Map as Map
import           GHC.Generics
import Lucid.Js (ToJExpr(..))

data XY = XY { xXY:: Double, yXY:: Double } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON XY
instance ToJSON XY

instance ToJExpr XY where
    toJExpr (XY x y) =
        toJExpr
        (Map.fromList
         [ ("xXY", x)
         , ("yXY", y)
         ] :: Map.Map String Double)

