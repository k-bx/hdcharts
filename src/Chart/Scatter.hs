{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- http://bl.ocks.org/mbostock/3048450

module Chart.Scatter where

import           Chart.Render
import           Chart.Types
import           Data.Default
import           Data.Random
import           Data.Text (pack)
import           Data.XY
import           Lucid.Css (Css,(?))
import qualified Lucid.Css as Css
import           Lucid.Js as Js
import           MVC
import qualified Pipes.Prelude as Pipes
import           Pipes.Util
import           Web.Play

scatterCss :: String -> Css
scatterCss cl =
  Css.element (pack cl<>" circle") ? do
    Css.stroke Css.steelblue
    Css.fill Css.white

circleScale :: Double -> [D3Expr]
circleScale radius = 
      toD3Expr [jmacroE|attr("r", `(radius)`)|]
  <.> toD3Expr [jmacroE|attr("cx", function(d) { return c.xScale(d.xXY); })|]
  <.> toD3Expr [jmacroE|attr("cy", function(d) { return c.yScale(d.yXY); })|]

testElement :: Element Double
testElement =
  Element
  "scatter0"
  "scatter"
  "scatter0"
  (defaultCreate (zipWith XY [0..5] [0..5]) "circle" (circleScale 4))
  (scatterCss ".scatter")
  mempty
  (defaultUpdate DataAdd "circle" (circleScale 4))
  defaultExprUC

testProducer :: Producer XY IO ()
testProducer = rvP 0 1 >-> addCountP >-> Pipes.map (\(x,y) -> XY (fromIntegral x) y)

-- http://localhost:8001/
testPlay :: IO ()
testPlay = void $
  runPlay
  testProducer
  (Chart.Render.render (chartCss "chart") [defaultChart [testElement]])
  def
