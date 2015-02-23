{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- bar chart with variable width of rectangles

module Chart.BarW where

import           Chart.Render
import           Chart.Types
import Control.Applicative
import           Control.Lens hiding ((<.>), Action)
import           Control.Monad
import           Data.Default
import           Data.Monoid
import           Data.Random
import           Data.XYZ
import qualified Lucid.Css as Css
import Lucid.Css (Css, (?))
import           Lucid.Js as Js
import           Pipes
import qualified Pipes.Prelude as Pipes
import           Web.Play

data BarWConfig =
  BarWConfig
  { _cTickStyle :: Maybe TickStyle
  , _cSeed :: [Double]
  }

makeLenses ''BarWConfig

instance Default BarWConfig where
  def = BarWConfig (Just (TickStyle Middle Nothing)) []

barWCss :: String -> Css
barWCss s = do
  Css.element (Css.pack s<>" rect") ? do
    Css.shapeRendering Css.crispEdges
    Css.fill Css.steelblue
  "path.domain" ? Css.opacity 0.5
  "g.tick" ? do
    "line" ? Css.stroke Css.black
    "text" ? Css.fontSize (Css.px 10)

barWRectScale :: [D3Expr]
barWRectScale = 
  toD3Expr $
  [jmacroE|
   attr("x", function(d) { 
     return c.xScale(d._xXYZ);}).
   attr('y', function(d) { 
     return c.yScale(d._yXYZ);}).
   attr("width", function(d) { 
     return c.xScale(d._xXYZ + d._zXYZ) - c.xScale(d._xXYZ);}).
   attr("height", function(d) { 
     return (c.height - c.yScale(d._yXYZ));})
  |]

barWCreate :: BarWConfig -> [Dim Double] -> Element Double -> JExpr
barWCreate c dims e =
  wrapF "c" $ mconcat
  [ dataCreate (c^.cSeed) e
  , barWExtents dims e
  , scales dims 
  , barWAxesUpdate (c^.cTickStyle) Setup dims e 
  , elCreate "rect" barWRectScale e
  ]

barWExtents :: (ToJExpr a) => [Dim a] -> Element a -> JStat
barWExtents dims e = mconcat $ barWExtent e <$> dims

barWExtent :: (ToJExpr a) => Element a -> Dim a -> JStat
barWExtent e dim =
  case dim of
    Dim "x" min' max' -> extentX' e min' max'
    Dim "y" min' max' -> extentY' e min' max'

extentX' :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentX' e min' max' = 
  extent' "x"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d._xXYZ})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d._xXYZ+d._zXYZ})|]

extentY' :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentY' e min' max' =
  extent' "y"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d._yXYZ})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d._yXYZ})|]

barWAxesUpdate :: Maybe TickStyle -> Action -> [Dim Double] -> Element Double -> JStat
barWAxesUpdate ts action dims e = 
  case ts of
    Nothing -> axesUpdate dims action
    Just ts' -> barWMakeTickValues ts' e <> barWOrdTicks ts' e <> axisOrdUpdateX action <> axisUpdate Setup "y"

barWMakeTickValues :: TickStyle -> Element Double -> JStat
barWMakeTickValues ts e =
 case ts of
   TickStyle Middle _ -> 
     [jmacro|
      var !vals = [];
      for (var i=0;i < `(datE e)`.length;i++) {
        vals[i] = `(datE e)`[i]._xXYZ+0.5*`(datE e)`[i]._zXYZ
      }
     |]
   TickStyle Ends _ -> 
     [jmacro|
      var !vals = [];
      for (var i=0;i < `(datE e)`.length;i++) {
        vals[i] = `(datE e)`[i]._xXYZ
      }
      vals[`(datE e)`.length] = `(datE e)`[i]._xXYZ+`(datE e)`[i]._zXYZ;
     |]

barWOrdTicks :: TickStyle -> Element Double -> JStat
barWOrdTicks ts e =
  barWMakeTickValues ts e <>
  [jmacro| c.axisord = `(d3stats)`; |]
  where
    d3stats = toJExpr $
      d3ScaleX <.>
      toD3Expr [jmacroE| tickValues(vals)|] <.>
      toD3Expr (tickFormat ts)
    tickFormat (TickStyle _ Nothing) =
      [jmacroE|
       tickFormat(d3.format(".2g"))
      |]
    tickFormat (TickStyle _ (Just ls)) =
      [jmacroE|
       tickFormat(function (d) {
         var labels;
         labels = `(ls)`;
         if (d>=labels.length) {
           var f = d3.format(".2g");
           return f(d);
         } else {
           return labels[d];
         }
       })
      |]

barWUpdate :: BarWConfig -> [Dim Double] -> Element Double -> JExpr
barWUpdate c dims e =
  wrapF2 "c" "d" $ mconcat
  [ dataUpdateReplace e
  , barWExtents dims e
  , scales dims 
  , barWAxesUpdate (c^.cTickStyle) Update dims e 
  , elUpdate "rect" barWRectScale e
  ]

testProducer :: Producer [XYZ] IO ()
testProducer =
  toHistP 10 (-3) 3 0.001 0.95 (rvP 0 1) >->
  Pipes.drop 1

testElement :: Element Double
testElement = 
  Element
  "barwe"
  "barw"
  "barw0"
  (barWCreate def)
  (barWCss ".barw")
  mempty
  (barWUpdate def)
  defaultExprUC

testPlay :: IO ()
testPlay = void $
  runPlay
  testProducer
  (Chart.Render.render (chartCss "chart") [defaultChart [testElement]])
  def

histAdaptProducer :: Producer [XYZ] IO ()
histAdaptProducer =
    toHistAdaptP 0.05 0.1 10 0.98 (rvP 0 1) >->
    Pipes.drop 1 >->
    Pipes.map normYOnZ

testPlayAdapt :: IO ()
testPlayAdapt = void $
  runPlay
  histAdaptProducer
  (Chart.Render.render (chartCss "chart") [testChartAdapt])
  def

testChartAdapt :: UpdateableChart Double
testChartAdapt = 
  ucChart . cDims .~
  [ Dim "x" Nothing Nothing 
  , Dim "y" (Just 0) Nothing
  ]
  $ defaultChart [testElementAdapt]

testElementAdapt :: Element Double
testElementAdapt = let c = cTickStyle .~ Nothing $ def in
  Element
  "barwe"
  "barw"
  "barw0"
  (barWCreate c)
  (barWCss ".barw")
  mempty
  (barWUpdate c)
  defaultExprUC
