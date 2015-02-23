{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

-- data type for a chart with three dimensions/scales
-- a common type is a histogram where x is horizontal, y is vertical height, and z is width
-- but it can be anything

module Data.XYZ where

import           Control.Applicative
import qualified Control.Foldl as L
import           Control.Foldl.Incremental.Histogram
import           Control.Lens
import           Data.Aeson
import           Data.Data
import           Data.Histogram
import qualified Data.Map as Map
import           Data.Random
import qualified Data.Vector.Unboxed as V
import           GHC.Generics
import           Language.Javascript.JMacro
import           Pipes
import qualified Pipes.Prelude as Pipes

-- underlying data type
data XYZ = XYZ 
  { _xXYZ :: Double -- x coordinate
  , _yXYZ :: Double -- y coordinate
  , _zXYZ :: Double -- z coordinate
  } deriving (Show, Eq, Data, Typeable, Generic)

makeLenses ''XYZ

instance FromJSON XYZ
instance ToJSON XYZ

instance ToJExpr XYZ where
  toJExpr (XYZ x y z) =
    toJExpr
    (Map.fromList
     [ ("_xXYZ", x)
     , ("_yXYZ", y)
     , ("_zXYZ", z)
     ] :: Map.Map String Double)

-- manipulations
toHist :: [Double] -> [XYZ]
toHist ns =
    let ns' = (max (-3) . min 2.99) <$> ns
        h = L.fold (incAdaptiveHist 0.05 0.01 30 0.98) ns'
    in
    hist2xyz h

hist2xyz :: (Bin b, IntervalBin b, BinValue b ~ Double) => Histogram b Double-> [XYZ]
hist2xyz h = zipWith (\v (l,u) -> XYZ l v (u-l))
               (V.toList $ histData h)
               (V.toList $ binsList $ bins h :: [(Double,Double)])

toHistP :: Int -> Double -> Double -> Double -> Double -> Producer Double IO () -> Producer [XYZ] IO ()
toHistP grain mini maxi epsilon decay p =
    p >->
    Pipes.map (max mini . min (maxi - epsilon)) >->
    L.purely Pipes.scan (incHist (binD mini grain maxi) decay) >->
    Pipes.map hist2xyz

toHistAdaptP :: (Monad m) => Double -> Double -> Int -> Double -> Producer Double m () ->
               Producer [XYZ] m ()
toHistAdaptP thresh grain maxBins decay p =
    p >->
    L.purely Pipes.scan (incAdaptiveHist thresh grain maxBins decay) >->
    Pipes.map hist2xyz

-- concrete examples
hbar0 :: Int -> Double -> Double -> Double -> [XYZ]
hbar0 grain mini maxi decay =
    hist2xyz $ L.fold (incHist (binD mini grain maxi) decay) []

hbarExample :: Int -> Double -> Double -> Int -> Double -> IO [XYZ]
hbarExample n thresh grain maxBins decay = do
  rvs <- rv n
  return $ hist2xyz $ L.fold (incAdaptiveHist thresh grain maxBins decay) rvs

-- functions
avZ :: [XYZ] -> Double
avZ bs = L.fold L.sum ((\(XYZ _ _ z) -> z) <$> bs) / fromIntegral (length bs)

normYOnZ :: [XYZ] -> [XYZ]
normYOnZ bs = (\(XYZ x y z) -> XYZ x (y/z*avZ bs) z) <$> bs



