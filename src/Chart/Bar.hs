{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Chart.Bar where

import           Chart.Render
import           Chart.Types
import           Control.Applicative
import           Control.Lens hiding ((<.>))
import           Control.Monad
import           Data.Default
import           Data.Monoid
import           Data.Random
import           Lucid.Css (Css,(?))
import qualified Lucid.Css as Css
import           Lucid.Js as Js
import           Pipes
import           Web.Play

data BarConfig =
  BarConfig
  { _bcWidth :: Width
  , _bcTickStyle :: Maybe TickStyle
  , _bcSeed :: [Double]
  }

makeLenses ''BarConfig

instance Default BarConfig where
  def = BarConfig (AutoWidth 1) (Just (TickStyle Middle Nothing)) []

barCss :: String -> Css
barCss s = do
  Css.element (Css.pack s<>" rect") ? do
    Css.shapeRendering Css.crispEdges
    Css.fill Css.steelblue
  "path.domain" ? Css.opacity 0.5
  "g.tick" ? do
    "line" ? Css.stroke Css.black
    "text" ? Css.fontSize (Css.px 10)

barExtents :: BarConfig -> [Dim Double] -> Element Double -> JStat
barExtents c dims e = mconcat $ barExtent e <$> dims
  where
    barExtent e dim =
      case dim of
        Dim "x" _ _ -> extent' "x" (toJExpr (0::Double))
          [jmacroE|
              d3.max([c.xmax,`(datE e)`.length])
          |]
        Dim "y" min' max' -> extent' "y" 
          [jmacroE|d3.min(`(datE e)`)|]
          [jmacroE|d3.max(`(datE e)`)|]

barScales :: BarConfig -> Element Double -> JStat
barScales c e =
  let scaleX' =
        case c^.bcWidth of
          AutoWidth _ -> scaleX
          ManualWidth offset width gap ->
            [jmacro|
             var l = `(datE e)`.length;
             c.xScale = 
               d3.scale.linear().
               domain([c.xmin, c.xmax]).
               range([`(offset)`, `(width+gap)`*l]);
            |]
  in
  scaleX' <> scaleY

barAxesUpdate :: Maybe TickStyle -> Action -> [Dim Double] -> Element Double -> JStat
barAxesUpdate ts action dims e = 
  case ts of
    Nothing -> axesUpdate dims action
    Just ts' -> barOrdTicks ts' e <> axisOrdUpdateX action <> axisUpdate Setup "y"

barOrdTicks :: TickStyle -> Element Double -> JStat
barOrdTicks ts e =
  [jmacro| c.axisord = `(d3stats)`; |]
  where
    d3stats = toJExpr $
      d3ScaleX <.>
      tickValues ts <.>
      tickFormat ts
    tickValues (TickStyle Middle _) =
      toD3Expr [jmacroE|tickValues(d3.range(0.5,`(datE e)`.length,1))|]
    tickValues (TickStyle Ends _) =
      toD3Expr [jmacroE|tickValues(d3.range(0,`(datE e)`.length+1,1))|]
    tickFormat (TickStyle Middle Nothing) = toD3Expr
      [jmacroE|
       tickFormat(function (d) {
         var f = d3.format("d");
         return f(d-0.5);
       })
      |]
    tickFormat (TickStyle Ends Nothing) = toD3Expr
      [jmacroE|
       tickFormat(function (d) {
         var f = d3.format("d");
         return f(d);
       })
      |]
    tickFormat (TickStyle _ (Just ls)) = toD3Expr
      [jmacroE|
       tickFormat(function (d) {
         var labels; 
         labels = `(ls)`;
         if (d>=labels.length) {
           var f = d3.format("d");
           return f(d);
         } else {
           return labels[d];
         }
       })
      |]

barWidth :: BarConfig -> Element Double -> JExpr
barWidth c e = 
  let gap' = r $ "c."<>e^.eName<>".gap" in
  case c^.bcWidth of
    AutoWidth _ -> [jmacroE| c.width / `(datE e)`.length - `(gap')`|] 
    ManualWidth _ w _ -> toJExpr w

barGap :: BarConfig -> Element Double -> JStat
barGap c e =
  let gap' = r $ "c."<>e^.eName<>".gap"
      jgap g = [jmacro| `(gap')` = `(g)`;|]
  in
  case c^.bcWidth of
    AutoWidth gap -> jgap gap
    ManualWidth _ _ gap -> jgap gap

barRectScale :: BarConfig -> Element Double -> [D3Expr]
barRectScale c e = 
  toD3Expr $
  [jmacroE|
   attr("x", function(d,i) { 
     return c.xScale(i);}).
   attr('y', function(d) { 
     return c.yScale(d);}).
   attr("width", function(d) { 
     return `(barWidth c e)`;}).
   attr("height", function(d) { 
     return (c.height - c.yScale(d));})
  |]

barCreate :: BarConfig -> [Dim Double] -> Element Double -> JExpr
barCreate c dims e =
  wrapF "c" $ mconcat
  [ dataCreate (c^.bcSeed) e
  , barGap c e
  , barExtents c dims e
  , barScales c e 
  , barAxesUpdate (c^.bcTickStyle) Setup dims e
  , elCreate "rect" (barRectScale c e) e
  ]

barUpdate :: BarConfig -> [Dim Double] -> Element Double -> JExpr
barUpdate c dims e =
  wrapF2 "c" "d" $ mconcat
  [ dataUpdateReplace e
  , barExtents c dims e
  , barScales c e
  , barAxesUpdate (c^.bcTickStyle) Update dims e
  , elUpdate "rect" (barRectScale c e) e
  ]

testProducer :: Producer [Double] IO ()
testProducer = toBar 6 (rvP 0 1)

toBar
  :: (Monad m)
  => Int
  -> Producer Double m ()
  -> Producer [Double] m ()
toBar n p =
  p >-> forever (yield =<< replicateM n await)

testElement :: Element Double
testElement = 
  Element
  "bare"
  "bar"
  "bar0"
  (barCreate def)
  (barCss ".bar")
  mempty
  (barUpdate def)
  defaultExprUC

testPlay0 :: IO ()
testPlay0 = void $
  runPlay
  testProducer
  (Chart.Render.render    (chartCss "chart") 
   [defaultChart [testElement]])
  def

testPlay :: IO ()
testPlay = void $
  runPlay
  testProducer
  (Chart.Render.render
   (chartCss "chart") 
   [defaultChart 
    [eCreate .~ barCreate 
     (bcSeed .~ [1, 2, 3, 4] $
      bcTickStyle .~ Just (TickStyle Ends (Just ["zero","one","two"]))
      $ def)
      $ testElement]])
  def
