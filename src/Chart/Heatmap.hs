{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Heatmap where

import           Data.Rect
import           Chart.Render
import           Chart.Types
import Control.Applicative
import           Control.Lens hiding ((<.>))
import           Data.Default
import           Data.Monoid
import Data.Text (pack)
import qualified Lucid.Css as Css
import Lucid.Css (Css, (?))
import           Lucid.Js
import           Pipes
import qualified Pipes.Prelude as Pipes
import           Web.Play

data Config =
    Config
    { _cGrain :: Double
    , _cMin :: Double
    , _CMax :: Double
    , _cEpsilon :: Double
    , _cCorr :: Double
    , _cDecayRate :: Double
    , _cSeed :: [Rect]
    }

makeLenses ''Config

instance Default Config where
    def = Config 2.0 (-3.0) 3.0 0.00001 0.6 0.99 (rectZero (-3.0) 3.0 2.0)

heatmapCss :: String -> Css
heatmapCss s = do
  Css.element (pack s<>" rect") ?
    Css.shapeRendering Css.crispEdges
  "path.domain" ? Css.opacity 0.5
  "g.tick" ? do
    "line" ? Css.stroke Css.black
    "text" ? Css.fontSize (Css.px 10)

rectScale :: [D3Expr]
rectScale = 
  toD3Expr $
  [jmacroE|
   attr("x", function(d) {
     return c.xScale(d._rectX);}).
   attr('y', function(d) { 
     return (c.height - c.yScale(d._rectY));}).
   attr("width", function(d) {
     return c.xScale(d._rectX+d._rectW)-c.xScale(d._rectX);}).
   attr("height", function(d) {
     return (c.yScale(d._rectY) - c.yScale(d._rectY+d._rectH));}).
   style("fill", function(d) { return c.zScale(d._rectZ); })
  |]

heatmapCreate :: [Rect] -> [Dim Double] -> Element Double -> JExpr
heatmapCreate seed dims e =
  wrapF "c" $ mconcat
  [ dataCreate seed e
  , heatmapExtents dims e
  , scales dims
  , axesUpdate dims Setup
  , elCreate "rect" rectScale e
  ]

heatmapExtents :: (ToJExpr a) => [Dim a] -> Element a -> JStat
heatmapExtents dims e = mconcat $ heatmapExtent e <$> dims

heatmapExtent :: (ToJExpr a) => Element a -> Dim a -> JStat
heatmapExtent e dim =
  case dim of
    Dim "x" min' max' -> extentX' e min' max'
    Dim "y" min' max' -> extentY' e min' max'
    Dim "z" min' max' -> extentZ' e min' max'

extentX' :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentX' e min' max' = 
  extent' "x"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d._rectX})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d._rectX + d._rectW})|]

extentY' :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentY' e min' max' = 
  extent' "y"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d._rectY})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d._rectY + d._rectH})|]

extentZ' :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentZ' e min' max' = 
  extent' "z"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d._rectZ})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d._rectZ})|]

heatmapUpdate :: [Dim Double] -> Element Double -> JExpr
heatmapUpdate dims e =
  wrapF2 "c" "d" $ mconcat
  [ dataUpdateReplace e
  , heatmapExtents dims e
  , scales dims 
  , axesUpdate dims Update 
  , elUpdate "rect" rectScale e
  ]

heatmapProducerValues :: Config -> Producer [Double] IO ()
heatmapProducerValues c =
    zValuesP
    (c^.cGrain) (c^.cCorr) (c^.cMin) (c^.cMax) (c^.cEpsilon) (c^.cDecayRate) >->
    Pipes.drop 1

heatmapProducer :: Config -> Producer [Rect] IO ()
heatmapProducer c =
    rectP
    (c^.cGrain) (c^.cCorr) (c^.cMin) (c^.cMax) (c^.cEpsilon) (c^.cDecayRate) >->
    Pipes.drop 1

testElement :: Element Double
testElement = 
  Element
  "heatmap"
  "heatmap"
  "heatmap0"
  (heatmapCreate [])
  (heatmapCss ".heatmap")
  mempty
  heatmapUpdate
  defaultExprUC

testPlay :: IO PlayState
testPlay =
  runPlay
  (heatmapProducer def)
  (Chart.Render.render (chartCss "chart") 
  [ ucChart . cDims .~
     [ Dim "x" Nothing Nothing 
     , Dim "y" Nothing Nothing
     , Dim "z" Nothing Nothing
     ]
  $ defaultChart [testElement]])
  def
