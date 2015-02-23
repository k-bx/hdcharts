{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{-|

A Rect has 5 different dimensions/aspects

an X and Y coordinate, assumed to be the lower left corner

a Width and Height coord.  Upper right corner is (X+Width,Y+Height)

a Z dimension, which is the "value" of the rectangle (translated to a color for instance) 

-}

module Data.Rect where

import           Control.Applicative
import qualified Control.Foldl as L
import           Control.Foldl.Incremental.Histogram
import           Control.Lens
import           Data.Aeson
import           Data.Data
import Data.Random
import           Data.Histogram
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import           GHC.Generics
import Lucid.Js (ToJExpr(..))
import           Pipes
import qualified Pipes.Prelude as Pipes


-- | 2D Histogram data (used for heatmap testing)
data Rect = Rect 
  { _rectX :: Double
  , _rectY :: Double
  , _rectW :: Double
  , _rectH :: Double
  , _rectZ :: Double
  } deriving (Show, Eq, Data, Typeable, Generic)

makeLenses ''Rect

instance FromJSON Rect
instance ToJSON Rect

instance ToJExpr Rect where
  toJExpr (Rect x y w h z) =
    toJExpr
    (Map.fromList
     [ ("_rectX", x)
     , ("_rectY", y)
     , ("_rectW", w)
     , ("_rectH", h)
     , ("_rectZ", z)
     ] :: Map.Map String Double)


zValuesP :: Double-> Double-> Double-> Double-> Double-> Double->
         Producer [Double] IO ()
zValuesP grain corr mini maxi epsilon rate =
    rvcorrP corr >->
    L.purely Pipes.scan (trimmedIncHist2D (mini,maxi) grain epsilon rate) >->
    Pipes.map convertValues
  where
    convertValues :: Histogram a Double -> [Double]
    convertValues = V.toList . histData

rectP :: Double-> Double-> Double-> Double-> Double-> Double->
               Producer [Rect] IO ()
rectP grain corr mini maxi epsilon rate =
    rvcorrP corr >->
    L.purely Pipes.scan (trimmedIncHist2D (mini,maxi) grain epsilon rate) >->
    Pipes.map hist2Rect

trimmedIncHist2D :: (Double, Double) -> Double -> Double -> Double ->
                    L.Fold (Double, Double) (Histogram (Bin2D BinD BinD) Double)
trimmedIncHist2D (mini,maxi) grain epsilon rate =
    L.premap trim2D (incHist2D bin rate)
  where
    trim2D (x0,x1) =
        ( max mini . min (maxi - epsilon) $ x0
        , max mini . min (maxi - epsilon) $ x1)

    bin = Bin2D (binDn mini grain maxi) (binDn mini grain maxi)

hist2Rect :: Histogram (Bin2D BinD BinD) Double -> [Rect]
hist2Rect h =
        let (Bin2D bx by) = bins h
            rx = V.toList $ binsList bx :: [(Double,Double)]
            ry = V.toList $ binsList by :: [(Double,Double)]
            r = [(x,y) | x <- rx, y <- ry]
            v = V.toList $ histData h
        in
        zipWith (\z ((lx,ux),(ly,uy)) -> Rect lx ly (ux-lx) (uy-ly) z) v r

rectZero :: Double -> Double -> Double -> [Rect]
rectZero mini maxi grain =
    let (Bin2D bx by) = Bin2D (binDn mini grain maxi) (binDn mini grain maxi)
        rx = V.toList $ binsList bx :: [(Double,Double)]
        ry = V.toList $ binsList by :: [(Double,Double)]
        r = [(x,y) | x <- rx, y <- ry]
        v = replicate (length r) 0
    in
    zipWith (\z ((lx,ux),(ly,uy)) -> Rect lx ly (ux-lx) (uy-ly) z) v r

rangeX :: [Rect] -> (Double,Double)
rangeX h
    | null h = (0,0)
    | otherwise =
        ( fromMaybe 0 (L.fold L.minimum (view rectX <$> h))
        , fromMaybe 0 (L.fold L.maximum ((\x -> view rectX x + view rectW x) <$> h))
        )

rangeY :: [Rect] -> (Double,Double)
rangeY h
    | null h = (0,0)
    | otherwise =
        ( fromMaybe 0 (L.fold L.minimum (view rectY <$> h))
        , fromMaybe 0 (L.fold L.maximum ((\x -> view rectY x + view rectH x) <$> h))
        )

rangeZ :: [Rect] -> (Double,Double)
rangeZ h
    | null h = (0,0)
    | otherwise =
        ( fromMaybe 0 (L.fold L.minimum (view rectZ <$> h))
        , fromMaybe 0 (L.fold L.maximum (view rectZ <$> h))
        )
