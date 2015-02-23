{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Text where

import           Chart.Render
import           Chart.Types
import           Control.Applicative
import           Control.Lens hiding (element, (<.>))
import           Data.Default
import           Data.Monoid
import qualified Lucid.Css as Css
import           Lucid.Css (Css,(?),px)
import           Lucid.Js
import           Pipes
import           Web.Play

data Config =
  Config
  { _cX :: Int
  , _cY :: Int
  , _cValue :: String
  }

makeLenses ''Config

instance Default Config where
  def = Config 30 30 "this is a text chart"

textCss :: String -> Css
textCss name =
  Css.element (Css.pack name) ? do
    Css.fill Css.steelblue
    Css.fontSize (px 10)
    Css.fontFamily ["Arial", "Helvetica"] [Css.sansSerif]

textCreate :: Config -> Element a -> JExpr
textCreate c e =
  wrapF "c" $
    elAssign e (csvg <.> select e <.> attrs e)
  where
    select e = toD3Expr [jmacroE|append('g').append('text')|]
    attrs  e = d3Class e <.>
               d3Id e <.>
               toD3Expr [jmacroE|text(`(c^.cValue)`)|] <.>
               toD3Expr [jmacroE|attr('x', `(c^.cX)`)|] <.>
               toD3Expr [jmacroE|attr('y', `(c^.cY)`)|]

textUpdate :: Element a -> JExpr
textUpdate e = 
  wrapF2 "c" "d" $
    toStat (csvg <.> toD3Expr 
            [jmacroE|
             select(`("#"<>(e^.eId))`).text(d)
            |]
           )

testElement :: Element a
testElement =
  Element
  "text0"
  "text"
  "textid0"
  (const $ textCreate def)
  (textCss ".text")
  mempty
  (const textUpdate)
  defaultExprUC

testChart :: UpdateableChart Double
testChart = ucChart . cDims .~ [] $ defaultChart [testElement]

-- | two separate charts (one element in each)
testCharts :: [UpdateableChart Double]
testCharts =
  [ ucExprUpdate .~ "d[0]"
  $ ucChart . cName .~ "textc0"
  $ ucChart . cElements .~ 
    [ eCreate.~ const (textCreate (Config 30 30 "first"))
    $ eName .~ "text0"
    $ eId .~ "textid0"
    $ eCss .~ 
        Css.element (Css.pack "#textid0") ?
          Css.fontSize (px 14)
    $ testElement
    ]
  $ testChart
  , ucExprUpdate .~ "d.slice(1,3)"
  $ ucChart . cName .~ "textc1"
  $ ucChart . cElements .~ 
    [ eCreate.~ const (textCreate (Config 50 50 "second"))
    $ eName .~ "text1" 
    $ eUpdateExpr .~ "d[0]" 
    $ eId .~ "textid1"
    $ eCss .~ 
        Css.element (Css.pack "#textid1") ?
          Css.fill Css.red
    $ testElement
    , eCreate.~ const (textCreate (Config 0 0 "third"))
    $ eName .~ "text2" 
    $ eUpdateExpr .~ "d[1]" 
    $ eId .~ "textid2"
    $ eCss .~ 
        Css.element (Css.pack "#textid2") ?
          Css.fill Css.blue
    $ testElement
    ]
  $ testChart
  ]

testProducer :: Producer' String IO ()
testProducer =
  Pipes.each (replicate 3 <$> ['a'..'z'::Char])
  
-- http://localhost:8001/
testPlay :: IO ()
testPlay = void $ 
  runPlay
  testProducer
  (Chart.Render.render (chartCss "chart") testCharts)
  def
