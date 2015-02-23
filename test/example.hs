{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Chart.Bar as Bar
import qualified Chart.BarW as BarW
import qualified Chart.Combo as Combo
import qualified Chart.Heatmap as Heatmap
import qualified Chart.Line as Line
import           Chart.Render
import qualified Chart.Scatter as Scatter
import qualified Chart.Text as ChartText
import           Chart.Types
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Text (Text)
import           Data.XY
import           Happstack.Server
import           Lucid
import           Lucid.Server
import           MVC
import           Pipes
import           Web.Play

main :: IO ()
main = serve testCharts

testCharts :: ServerPart Response
testCharts = msum
  [ dir "text" textChart
  , dir "line" lineChart
  , dir "circle" circleChart
  , dir "scroll" scrollChart
  , dir "scatter" scatterChart
  , dir "heatmap" heatmapChart
  , dir "bar" barChart
  , dir "hist" histChart
  , dir "histadapt" histAdaptChart
  , dir "combo" comboChart
  , homePage
  ]

template :: Text -> Html () -> Response
template title' body' = toResponse
  (html_
  (head_ (title_ (toHtml title'))
   <> body_
   (body' <>
             p_ (with a_ [href_ "/"] "home"))) :: Html ())

homePage :: ServerPart Response
homePage =
    ok $ template "home page" $ mconcat
           [ h1_ "Chart tests"
           , p_ (with a_ [href_ "/text"] "text")
           , p_ (with a_ [href_ "/line"     ] "line chart")
           , p_ (with a_ [href_ "/scroll"     ] "scroll chart")
           , p_ (with a_ [href_ "/circle"     ] "circle")
           , p_ (with a_ [href_ "/scatter"  ] "scatter chart")
           , p_ (with a_ [href_ "/heatmap"  ] "heatmap chart")
           , p_ (with a_ [href_ "/bar"] "bar chart")
           , p_ (with a_ [href_ "/hist"] "hist chart")
           , p_ (with a_ [href_ "/histadapt"] "histadapt chart")
           , p_ (with a_ [href_ "/combo"    ] "combo test")
           ]

textChart :: ServerPartT IO Response
textChart =
  responsePlay
  ChartText.testProducer 
  (Chart.Render.render (chartCss "chart") ChartText.testCharts)
  def

lineChart :: ServerPart Response
lineChart =
  responsePlay
  Line.testProducer 
  (Chart.Render.render (chartCss "chart") [defaultChart [Line.testElement]])
  def

circleChart :: ServerPart Response
circleChart =
  responsePlay
  (Pipes.each $ zipWith (\a b -> [a,b])
   ((\x -> XY (cos x) (sin x)) <$> (/20) <$> [0..])
   ((\x -> XY (sin (x + pi/2)) (sin (3*x))) <$> (/20) <$> [0..]))
  (Chart.Render.render (chartCss "chart") Line.testChart2)
  def

scrollChart :: ServerPart Response
scrollChart =
  responsePlay
  Line.testProducer
  (Chart.Render.render (chartCss "chart") [defaultChart [Line.testElementScroll]])
  def

scatterChart :: ServerPart Response
scatterChart =
  responsePlay
  Scatter.testProducer
  (Chart.Render.render (chartCss "chart") [defaultChart [Scatter.testElement]])
  def

heatmapChart :: ServerPart Response
heatmapChart =
  responsePlay
    (Heatmap.heatmapProducer def)
    (Chart.Render.render (chartCss "chart") 
     [ ucChart . cDims .~
       [ Dim "x" Nothing Nothing 
       , Dim "y" Nothing Nothing
       , Dim "z" Nothing Nothing
       ]
       $ defaultChart [Heatmap.testElement]])
    def

barChart :: ServerPart Response
barChart = 
  responsePlay
    Bar.testProducer
    (Chart.Render.render
     (chartCss "chart") 
     [defaultChart 
      [eCreate .~ Bar.barCreate 
       (Bar.bcSeed .~ [1, 2, 3, 4] $
        Bar.bcTickStyle .~ Just (TickStyle Ends (Just ["zero","one","two"]))
        $ def)
       $ Bar.testElement]])
    def

histChart :: ServerPart Response
histChart = 
  responsePlay
    BarW.testProducer
    (Chart.Render.render (chartCss "chart") [defaultChart [BarW.testElement]])
    def

histAdaptChart :: ServerPart Response
histAdaptChart = 
  responsePlay
    BarW.histAdaptProducer
    (Chart.Render.render (chartCss "chart") [BarW.testChartAdapt])
    def

comboChart :: ServerPart Response
comboChart = 
  responsePlay (Combo.comboProducer def) Combo.comboPage def


