{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Render where

import Chart.Types
import Control.Applicative
import Control.Lens
import Data.Default
import Data.Monoid
import Lucid
import Lucid.Css as Css
import Lucid.Js as Js
import Web.Page
import Web.Play.Types
import Web.Socket

defaultCssLibs :: [String]
defaultCssLibs = []

defaultJsLibs :: [String]
defaultJsLibs = 
    [ "http://d3js.org/d3.v3.min.js"
    , "http://code.jquery.com/jquery-1.6.3.min.js"
    ]

chartPage :: [Chart a] -> Page
chartPage charts =
    let csss = view cCss <$> charts
        htmls = view cHtml <$> charts
    in
    chartLibs <> Page [] [] (mconcat csss) (decl charts) (stats charts) mempty (mconcat htmls)

-- combinator
ce :: Chart a -> Element a -> String
ce c e  = (c^.cName) <> "." <> (e^.eName)

setupCharts :: Chart a -> JStat
setupCharts c = 
  [jmacro| 
   `(r ((c^.cName) <> ".setup"))` = `((c^.cCreate) c)`; 
  |]

setupElements :: Chart a -> JStat
setupElements c = mconcat $ setupElement c <$> (c^.cElements)
  where
    setupElement c e = r (ce c e <> ".setup") =: (e^.eCreate) (c^.cDims) e

applyChartSetup :: Chart a -> JStat
applyChartSetup c = 
  r (c^.cName) =: (r (c^.cName <> ".setup") $$ [r (c^.cName)])

applyElementSetups :: Chart a -> JStat
applyElementSetups c = 
  mconcat $
  (\e ->
    r (c^.cName) =:
    (
      r (ce c e <> ".setup") $$
      [r (c^.cName)]
    )) <$> c^.cElements

decl :: [Chart a] -> JStat
decl charts = mconcat (declChart <$> charts)
  where
    declChart c = 
      (declObj . view cName) c <>
      declElements c
    declElements c = mconcat (declElement c <$> c^.cElements)
    declElement c e = r (ce c e) =: r "{}"

setup :: Chart a -> JStat
setup c = 
  mconcat 
  [ setupCharts c
  , setupElements c
  , applyChartSetup c
  , applyElementSetups c
  ]

-- global chart statements
stats :: [Chart a] -> JStat
stats charts = mconcat (setup <$> charts)

ucPage :: SocketConfig -> PlayState -> [UpdateableChart a] -> Page
ucPage sc p ucharts =
    let charts = view ucChart <$> ucharts
        csss = view cCss <$> charts
        htmls = view cHtml <$> charts
        handleStream = r "play.handleStream" =: (ValExpr $ JFunc [StrI "d"] (BlockStat applyUpdates))
        names      = view cName <$> charts
        chartExtracts = view ucExprUpdate <$> ucharts
        applyUpdates =
            zipWith ($$$)
            (r . (<> ".update") <$> names)
            ((:[]) <$> chartExtracts)

    in
    play handleStream p sc <> Page [] [] (mconcat csss) (updateDecl ucharts) (updateStats ucharts) mempty (mconcat htmls)

updateDecl :: [UpdateableChart a] -> JStat
updateDecl ucharts =
    let charts     = view ucChart <$> ucharts
        nameE c e  = (c^.cName) <> "." <> (e^.eName)
        declareC c = declObj (c^.(ucChart.cName))
        declareE c = (\e -> AssignStat (r (nameE c e)) (jsv "{}")) <$>
                     c^.cElements
        declares = mconcat (declareC <$> ucharts) <>
                   mconcat (mconcat (declareE <$> charts))

        in
    declares

updateStats :: [UpdateableChart a] -> JStat
updateStats ucharts = mconcat (ustat <$> ucharts)
  where
    ustat uc = let c = uc^.ucChart in
      mconcat 
      [ setup c
      , updateChart uc
      , updateAssignElements c
      ]
    updateChart uc = 
                 r ((uc^.(ucChart.cName)) <> ".update") =:
                 (uc^.ucUpdate) uc
    updateElement dim e = view eUpdate e dim e
    updateElements c = updateElement (c^.cDims) <$> (c^.cElements)
    enames c = ce c <$> (c^.cElements) 
    updateAssignElements c = mconcat $ 
      zipWith
      (=:)
      (r <$> (<> ".update") <$> enames c)
      (updateElements c)


-- * renderers
renderC :: Css -> [Chart a] -> Html ()
renderC pagecss charts = renderPage $ chartPage charts <> Page [] [] pagecss mempty mempty mempty mempty 

renderUC :: SocketConfig -> PlayState -> Css -> [UpdateableChart a] -> Html ()
renderUC sc p pagecss ucharts =
    renderPage $ ucPage sc p ucharts <>
    Page [] [] pagecss mempty mempty mempty mempty

render :: Css -> [UpdateableChart a] -> Html ()
render css = renderUC def def css 

