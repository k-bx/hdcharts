{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Line where

import           Chart.Render
import           Chart.Types
import Control.Applicative
import           Control.Lens hiding (each, (<.>))
import           Data.Default
import           Data.Monoid
import           Data.Random
import           Data.XY
import qualified Lucid.Css as Css
import           Lucid.Css (Css,px,(?))
import           Lucid.Js
import           Pipes
import qualified Pipes.Prelude as Pipes
import           Pipes.Util
import           Web.Play

data Config =
    Config
    { _cMaxPoints :: Maybe Int
    , _cSeed :: [XY]
    }

makeLenses ''Config

instance Default Config where
  def = Config Nothing []

lineCss :: String -> Css
lineCss s =
  Css.element (Css.pack s) ? do
    Css.fill Css.none
    Css.stroke Css.steelblue;
    Css.strokeWidth (px 2)

{-
todo:
scales, axes and dims, which are now chart-level concepts, could also be element concepts.

to make that so:
- loop through elements calcing extent
- call scale at chart level
- loop back through elements applying scale
- go back to chart level and call axes

-}
lineCreate :: (ToJExpr a) => Config -> [Dim a] -> Element a -> JExpr
lineCreate cfg dims e =
  wrapF "c" $ mconcat
  [ dataCreate seed e
  , extents dims e
  , scales dims
  , axesUpdate dims Setup
  , lineElCreate e
  ]
  where
    seed = case cfg^.cMaxPoints of
      Nothing -> cfg^.cSeed
      Just n -> take n (cfg^.cSeed)

d3ClineCreate :: JStat
d3ClineCreate =
  [jmacro|
   c.line = 
       d3.svg.line().
       x(function(d) { return c.xScale(d.xXY); }).
       y(function(d) { return c.yScale(d.yXY); });
  |]

d3cline :: [D3Expr]
d3cline = toD3Expr [jmacroE|attr('d',c.line)|]

lineElCreate :: Element a -> JStat
lineElCreate e = 
  d3ClineCreate <>
  elAssign e (csvg <.> select <.> d3datum e <.> attrs e)
  where
    select = toD3Expr [jmacroE|append('g').append('path')|]
    attrs  e = d3Class e <.> d3Id e <.> d3cline

lineUpdate :: (ToJExpr a) => Config -> [Dim a] -> Element a -> JExpr
lineUpdate c dims e =
  wrapF2 "c" "d" $ mconcat
  [ dataUpdateAdd e
  , extents dims e
  , scales dims
  , axesUpdate dims Setup
  , lineElUpdate c e
  ]

lineElUpdate :: Config -> Element a -> JStat
lineElUpdate c e = 
  case c^.cMaxPoints of
    Nothing -> 
      toStat 
      (d3e e <.> 
       d3transition <.> 
       d3cline)
    Just m -> 
      toStat (d3e e <.> d3datum e <.> d3cline <.> d3TransformNull) <>
      toStat (d3e e <.> d3transition <.> d3TranslateXY0 e) <>
      shiftData m e

shiftData :: Int -> Element a -> JStat
shiftData m e =
      [jmacro|
       if (`(datE e)`.length > `(m)`) {
         `(datE e)` = `(datE e)`.slice(`(datE e)`.length - `(m)`,`(datE e)`.length);
       };
      |]

testElement :: Element Double
testElement =
  Element
  "line0"
  "line"
  "line0"
  (lineCreate def)
  (lineCss ".line")
  mempty
  (lineUpdate def)
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

-- a circle
testChart2 :: [UpdateableChart Double]
testChart2 =
  [ ucChart . cElements .~
    [ eUpdateExpr .~ "d[0]" $ testElement
    , eUpdateExpr .~ "d[1]" 
    $ eName .~ "line1" 
    $ eId .~ "lineid1" 
    $ eCss .~ "#lineid1" ? Css.stroke Css.red 
    $ testElement
    ]
  $ defaultChart [testElement]
  ]

testCircle :: IO ()
testCircle = void $
  runPlay
  (each $ zipWith (\a b -> [a,b])
   ((\x -> XY (cos x) (sin x)) <$> (/20) <$> [0..])
   ((\x -> XY (sin (x + pi/2)) (sin (3*x))) <$> (/20) <$> [0..]))
  (Chart.Render.render (chartCss "chart") testChart2)
  def

testElementScroll :: Element Double
testElementScroll = let cfg = cMaxPoints .~ Just 5 $ def in 
    eCreate  .~ lineCreate cfg
  $ eUpdate .~ lineUpdate cfg
  $ testElement

-- | not transitioning properly
testScroll :: IO ()
testScroll = void $ 
  runPlay
  testProducer
  (Chart.Render.render (chartCss "chart") [defaultChart [testElementScroll]])
  def

setup1D :: [Double] -> Config -> [Dim Double] -> Element Double -> JExpr
setup1D seed c dims e =
    let seed' = zipWith XY seed [0..]
        c' = cSeed .~ seed' $ c
    in
    lineCreate c' dims e

update1D :: Config -> [Dim Double] -> Element Double -> JExpr
update1D c dims e =
  wrapF2 "c" "d" $ mconcat
  [ dataPush
  , extents dims e
  , scales dims
  , lineElUpdate c e
  , axesUpdate dims Update
  ]
  where
  dataPush =
        [jmacro|
         var lastX;
         if (`(datE e)`.length == 0) {
           lastX = 0; }
         else {
           lastX = `(datE e)`[`(datE e)`.length-1].xXY;
         };
         `(datE e)`.push({xXY:lastX+1, yXY:d});
        |]

testElement1D :: Element Double
testElement1D =
    eCreate .~ setup1D [0,1] def
  $ eUpdate .~ update1D def
  $ testElement

test1D :: IO ()
test1D = void $ 
  runPlay
  (rvP 0 1)
  (Chart.Render.render (chartCss "chart") [defaultChart [testElement1D]])
  def

