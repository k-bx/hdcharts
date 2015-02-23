{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Chart.Types where

import           Control.Applicative
import           Control.Lens hiding ((<.>), Action)
import           Data.Default
import           Data.Monoid
import           Language.Javascript.JMacro.D3Expr
import           Lucid
import           Lucid.Css (Css,px,(?))
import qualified Lucid.Css as Css
import           Lucid.Js as Js
import           Web.Socket

-- common types
data Dim a = Dim 
  { _dimName :: String
  , _dimLower :: Maybe a
  , _dimUpper :: Maybe a
  }

makeLenses ''Dim

data TickStyle = TickStyle TickPosition (Maybe [String])
data TickPosition = Middle | Ends
data Action = Setup | Update
data Width = AutoWidth Int | ManualWidth Int Int Int

-- base Type for turning data into a visual representation
data Element a =
    Element
    { _eName :: String
    , _eClass :: String
    , _eId :: String
    , _eCreate :: [Dim a] -> Element a -> JExpr
    , _eCss :: Css
    , _eHtml :: Html ()
    , _eUpdate :: [Dim a] -> Element a -> JExpr
    , _eUpdateExpr :: JExpr
    }

makeLenses ''Element

-- Chart Types and defaults
data Chart a =
    Chart
    { _cName :: String
    , _cClass :: String
    , _cId :: String
    , _cCreate :: Chart a -> JExpr
    , _cCss :: Css
    , _cHtml :: Html ()
    , _cElements :: [Element a]
    , _cWidth :: Int
    , _cHeight :: Int
    , _cMargins :: Css.Margins
    , _cDims :: [Dim a]
    }

makeLenses ''Chart

instance Default (Chart a) where
    def = 
      let dims = 
            [ Dim "x" Nothing Nothing
            , Dim "y" Nothing Nothing
            ]
      in 
      Chart "chart" "chart" "chartdef" chartCreate (chartCss "chart") mempty [] 300 300 (Css.Margins 30 30 30 30) dims

-- updateable chart types and chunks
-- data UpdateableChart a b =
data UpdateableChart a =
    UpdateableChart
    { _ucChart :: Chart a
    , _ucUpdate :: UpdateableChart a -> JExpr
    , _ucExtentNullify :: Bool
    , _ucExprUpdate :: JExpr
    , _ucSocket :: SocketConfig
    }

-- type UC = forall a b . UpdateableChart a b 

-- css
chartCss :: String -> Css
chartCss cl =
  Css.body ? Css.element ("."<>Css.pack cl) ? do
    Css.float Css.floatLeft
    Css.border Css.solid (px 1) Css.grey
    Css.margin
      (px 10)
      (px 10)
      (px 10)
      (px 10)
    -- Css.clear Css.clearLeft
    ".axis path, .axis line" ? do
      Css.fill Css.none
      Css.stroke "#000"
      Css.shapeRendering Css.crispEdges
    ".x.axis path" ?
      Css.display Css.none
    ".tick text" ? 
      Css.fontSize (px 8)

-- chart creation
chartCreate :: Chart a -> JExpr
chartCreate c =
    wrapF "c" $ mconcat
    [ sizingCreate c
    , svgCreate c
    -- , svgStyleCreate cssr
    , extentsNullify "c" c
    , axesCreate (c^.cDims)
    ]
--  where
--    cssr = Lazy.unpack $ Css.render (mconcat ((^.eCss) <$> (c^.cElements)))

-- | chart chunks
sizingCreate :: Chart a -> JStat
sizingCreate c = 
  [jmacro|
   c.margin = `(toJExpr (c^.cMargins))`;
   c.width  = `(c^.cWidth)`  - c.margin.left - c.margin.right;
   c.height = `(c^.cHeight)` - c.margin.top  - c.margin.bottom;
  |]

-- chart creation and setup
svgCreate :: Chart a -> JStat
svgCreate c =
     [jmacro|
      c.svg =
          d3.select("body").append("svg").
          attr("class", `(c^.cClass)`).
          attr("id", `(c^.cName)`).
          attr("width", c.width + c.margin.left + c.margin.right).
          attr("height", c.height + c.margin.top + c.margin.bottom).
          append("g").
          attr("transform", 
          "translate(" + c.margin.left + "," + c.margin.top + ")");
      |]

svgStyleCreate :: ToJExpr a =>  a -> JStat
svgStyleCreate css = 
     [jmacro|
      c.style = 
          c.svg.append("defs").append("style").
          attr("type","text/css").html(`(css)`);
     |]

-- * element chunks

-- element helpers
datE :: Element a -> JExpr
datE e = r $ "c."<> e^.eName <>".data"

elE :: Element a -> JExpr
elE e = r $ "c."<> e^.eName <>".element"

-- lower level d3 chunks
d3e :: Element a -> [D3Expr]
d3e = toD3Expr . elE

csvg :: [D3Expr]
csvg = toD3Expr [jmacroE|c.svg|]

d3SelClass :: Element a -> [D3Expr]
d3SelClass e = toD3Expr [jmacroE|selectAll(`("."<>e^.eClass)`)|]

d3SelClass' :: Element a -> String -> [D3Expr]
d3SelClass' e s = toD3Expr [jmacroE|selectAll(`("."<>e^.eClass<> " "<>s)`)|]

d3data :: Element a -> [D3Expr]
d3data e = toD3Expr [jmacroE|data(`(datE e)`)|]

d3datum :: Element a -> [D3Expr]
d3datum e = toD3Expr [jmacroE|data([`(datE e)`])|]

d3Class :: Element a -> [D3Expr]
d3Class e = toD3Expr [jmacroE|attr("class",`(e^.eClass)`)|]

d3Id :: Element a -> [D3Expr]
d3Id e = toD3Expr [jmacroE|attr("id",`(e^.eId)`)|]

d3g :: [D3Expr]
d3g = toD3Expr [jmacroE|append('g')|]

d3append :: String -> [D3Expr]
d3append s = toD3Expr [jmacroE|append(`(s)`)|]

d3enter :: [D3Expr]
d3enter = toD3Expr [jmacroE|enter()|]

d3exit :: [D3Expr]
d3exit = toD3Expr [jmacroE|exit()|]

d3remove :: [D3Expr]
d3remove = toD3Expr [jmacroE|remove()|]

d3transition :: [D3Expr]
d3transition = 
  toD3Expr 
  [jmacroE| 
   transition().ease('linear').duration(play.state._pSpeed*1000) 
  |]

d3TransformNull :: [D3Expr]
d3TransformNull = toD3Expr [jmacroE| attr("transform", null)|]

d3TranslateXY0 :: Element a -> [D3Expr]
d3TranslateXY0 e = toD3Expr [jmacroE|attr("transform", "translate(" + c.xScale(`(datE e)`[0].xXY) + ")")|]

d3axis :: [D3Expr]
d3axis = toD3Expr [jmacroE| d3.svg.axis()|]

d3ScaleX :: [D3Expr]
d3ScaleX = toD3Expr [jmacroE| d3.svg.axis().scale(c.xScale).orient("bottom")|]

d3ScaleY :: [D3Expr]
d3ScaleY = toD3Expr [jmacroE| d3.svg.axis().scale(c.yScale).orient("left")|]


-- extent
extents :: (ToJExpr a) => [Dim a] -> Element a -> JStat
extents dims e = mconcat $ extent e <$> dims

extent :: (ToJExpr a) => Element a -> Dim a -> JStat
extent e dim =
  case dim of
    Dim "x" min' max' -> extentX e min' max'
    Dim "y" min' max' -> extentY e min' max'

extentX :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentX e min' max' = 
  extent' "x"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d.xXY})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d.xXY})|]

extentY :: (ToJExpr a) => Element a -> Maybe a -> Maybe a -> JStat
extentY e min' max' =
  extent' "y"
      (maybe min'' toJExpr min')
      (maybe max'' toJExpr max')
  where
    min'' = [jmacroE|d3.min(`(datE e)`,function(d) {return d.yXY})|]
    max'' = [jmacroE|d3.max(`(datE e)`,function(d) {return d.yXY})|]

extent' :: (ToJExpr a) => String -> a -> a -> JStat
extent' dim min' max' =
  [jmacro|
   `(r $ "c."<>dim<>"min")` = d3.min([`(r $ "c."<>dim<>"min")`,`(min')`]);
   `(r $ "c."<>dim<>"max")` = d3.max([`(r $ "c."<>dim<>"max")`,`(max')`]);
  |]

extentsNullify :: String -> Chart a -> JStat
extentsNullify s c = mconcat $ extentNullify s <$> (c^.cDims)

extentNullify :: String -> Dim a -> JStat
extentNullify c d =
     [jmacro|
      `(r $ (c<>"."<>d^.dimName<>"min"))` = d3.min([]);
      `(r $ (c<>"."<>d^.dimName<>"max"))` = d3.max([]);
      |]

-- | scaling
scales :: [Dim a] -> JStat
scales dims = mconcat $ (scale' . view dimName) <$> dims
  where
    scale' "x" = scaleX
    scale' "y" = scaleY
    scale' "z" = scaleZ
    scale' _   = mempty

scale :: ToJExpr a => String -> a -> JStat
scale dim range =
  let dmin = r $ dim<>"min"
      dmax = r $ dim<>"max"
      dmin' = dim<>"min"
      dmax' = dim<>"max"
      cmin = r $ "c."<>dim<>"min"
      cmax = r $ "c."<>dim<>"max"
      sc = r $ "c."<>dim<>"Scale"
  in
  [jmacro|
   `(var dmin')`;
   `(var dmax')`;
   `(dmin)` = `(cmin)`;
   `(dmax)` = `(cmax)`;
   if (`(cmin)`==undefined) {`(dmin)` = 0;};
   if (`(cmax)`==undefined) {`(dmax)` = 1;};
   `(sc)` = 
     d3.scale.linear().
     domain([`(dmin)`, `(dmax)`]).
     range(`(range)`);
   |]

scaleX :: JStat
scaleX = scale "x" [jmacroE|[0, c.width]|]

scaleY :: JStat
scaleY = scale "y" [jmacroE|[c.height, 0]|]

scaleZ :: JStat
scaleZ = scale "z" [jmacroE|["white", "steelblue"]|]

-- | chart axis helpers
axesCreate :: [Dim a] -> JStat
axesCreate dims = mconcat $ (axisCreate . view dimName) <$> dims

axisCreate :: String -> JStat
axisCreate dim =
  case dim of
    "x" -> 
      [jmacro| 
       c.xaxis = 
       c.svg.append("g").
       attr("class", "x axis").
       attr("transform", "translate(0," + c.height + ")");
      |]
    "y" ->
     [jmacro|
      c.yaxis = 
      c.svg.append("g").
      attr("class", "y axis");
     |]
    _  -> mempty

axesUpdate :: [Dim a] -> Action -> JStat
axesUpdate dims a = mconcat $ axisUpdate a <$> view dimName <$> dims

axisUpdate :: Action -> String -> JStat
axisUpdate a d = 
  case d of
    "x" ->
      toStat $
      d3 "c.xaxis" <.>
      trans a <.> 
      toD3Expr [jmacroE|call(`(d3ScaleX)`)|]
    "y" ->
      toStat $ 
      d3 "c.yaxis" <.> 
      trans a <.>
      toD3Expr [jmacroE|call(`(d3ScaleY)`)|]
    _ -> mempty
  where
    trans Update = d3transition
    trans Setup = mempty

axisOrdUpdateX :: Action -> JStat
axisOrdUpdateX Setup = [jmacro| c.xaxis.call(c.axisord);|]
axisOrdUpdateX Update = toStat (d3 "c.xaxis" <.> d3transition <.> toD3Expr [jmacroE|call(c.axisord)|])

-- pushing data to javascript
dataUpdateAdd :: Element a -> JStat
dataUpdateAdd e = [jmacro| `(datE e)`.push(d); |]

dataUpdateReplace :: Element a -> JStat
dataUpdateReplace e = [jmacro| `(datE e)` = d;|]

-- fixme: sig,, some wierd overlapping instance issue
dataCreate :: (ToJExpr a, ToJExpr c) => c -> Element a -> JStat
dataCreate d e = [jmacro| `(datE e)` = `(d)`;|] 

-- | elements
-- pushing data from element to the right d3 selection, hooking in data and appending attributes

elAssign :: Element a -> [D3Expr] -> JStat
elAssign e d3chain =
  [jmacro|
   `(elE e)`=`(toJExpr d3chain)`;
  |]

elCreateClass :: Element a -> JStat
elCreateClass e = elAssign e 
  (csvg <.> d3SelClass e <.> d3data e)

elCreate :: String -> [D3Expr] -> Element a -> JStat
elCreate shape scaler e = 
  elAssign e 
  (csvg <.> 
   d3SelClass e <.>
   d3data e <.>
   d3enter <.> 
   d3g <.> 
   d3Class e <.> 
   d3append shape <.> 
   scaler
  )

elUpdate :: String -> [D3Expr] -> Element a -> JStat
elUpdate shape scaler e = data' <> update' <> enter' <> exit'
  where
    data' = elAssign e
      (csvg <.>
       d3SelClass' e shape <.>
       d3data e
      )
    update' = toStat 
      (d3e e <.> 
       d3transition <.>
       scaler
      )
    enter' = toStat 
      (d3e e <.>
       d3enter <.>
       d3g <.> 
       d3Class e <.> 
       d3append shape <.> 
       scaler
      )
    exit' = toStat 
      (d3e e <.> 
       d3exit <.> 
       d3remove
      )

-- create and update functions
defaultCreate :: (ToJExpr a, ToJExpr b) => b -> String -> [D3Expr] -> [Dim a] -> Element a -> JExpr
defaultCreate seed shape scaler dims e =
  wrapF "c" $ mconcat
  [ dataCreate seed e
  , extents dims e
  , scales dims
  , axesUpdate dims Setup
  , elCreate shape scaler e
  ]

data DataUpdate = DataReplace | DataAdd

defaultUpdate :: (ToJExpr a) => DataUpdate -> String -> [D3Expr] -> [Dim a] -> Element a -> JExpr
defaultUpdate du shape scaler dims e =
  wrapF2 "c" "d" $ mconcat
  [ case du of
      DataReplace -> dataUpdateReplace e
      DataAdd -> dataUpdateAdd e
  , extents dims e
  , scales dims
  , axesUpdate dims Update
  , elUpdate shape scaler e
  ]

-- updating
makeLenses ''UpdateableChart

instance Default (UpdateableChart a) where
    def = UpdateableChart def chartUpdate True defaultExprUC defaultSocketConfig

defaultChart :: (ToJExpr a) => [Element a] -> UpdateableChart a
defaultChart es =
  ucChart .~
  ( cWidth .~ 150
  $ cHeight .~ 150
  $ cMargins .~ Css.Margins 10 10 20 30
  $ cDims .~ 
  [ Dim "x" Nothing Nothing 
  , Dim "y" Nothing Nothing
  ]
  $ cClass .~ "chart"
  $ cCss .~ mconcat ((^.eCss) <$> es)
  $ cElements .~ es
  $ def
  )
  $ def

chartUpdate :: UpdateableChart a -> JExpr
chartUpdate uc =
    let es = uc^.ucChart.cElements
        chartName = uc^.ucChart.cName
        extracts = view eUpdateExpr <$> es
        names = view eName <$> es
        updates = 
          zipWith
                  ($$)
                  ((\n -> r (chartName <> "." <> n <> ".update")) <$> names)
                  ((\x -> [r chartName,x]) <$>
                   extracts)
        aUpdates = fmap (\x -> [jmacro|`(r chartName)`=`(x)`;|]) updates
    in
    wrapF0 "d" $ mconcat
    [ if uc^.ucExtentNullify
      then extentsNullify (uc^.ucChart.cName) (uc^.ucChart) 
      else mempty
    , BlockStat aUpdates
    ]

defaultExprUC :: JExpr
defaultExprUC = r "d"


