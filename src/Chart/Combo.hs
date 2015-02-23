{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- chart combination test

module Chart.Combo where

import qualified Chart.BarW as BarW
import qualified Chart.Heatmap as Heatmap
import qualified Chart.Line as Line
import           Chart.Render
import qualified Chart.Text as ChartText
import           Chart.Types
import qualified Control.Foldl as L
import           Control.Foldl.Incremental.Histogram
import           Control.Lens
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad
import           Data.Aeson
import           Data.Data
import           Data.Default
import           Data.Histogram
import           Data.Monoid
import           Data.Random
import           Data.Rect
import           Data.Text (Text, pack)
import           Data.Tuple (swap)
import           Data.XY
import           Data.XYZ
import           GHC.Generics
import           Lucid
import           Lucid.Css ((?))
import qualified Lucid.Css as Css
import           Lucid.Js
import           Pipes
import           Pipes.Internal (unsafeHoist)
import           Pipes.Lift (distribute)
import qualified Pipes.Prelude as Pipes
import           Web.Play

data Combo =
  Combo
  { comboText :: Text
  , comboLine :: XY
  , comboScroll :: XY
  , comboHeatmap :: [Rect]
  , comboHist :: [XYZ]
  , comboHistAdapt :: [XYZ]
  } deriving (Show, Eq, Data, Typeable, Generic)

instance FromJSON Combo
instance ToJSON Combo

data Config = Config
  { _corr :: Double
  , _mini :: Double
  , _maxi :: Double
  , _heatGrain :: Double
  , _decayRate :: Double
  , _eps :: Double
  , _maxBins :: Int
  , _thresh :: Double
  , _adaptGrain :: Double
  }

makeLenses ''Config

instance Default Config where
    def = Config 0.6 (-3) 3 1 0.95 0.0001 6 0.2 0.3

comboProducer :: Config -> Producer Combo IO ()
comboProducer c =
  rvcorrP (c^.corr) >->
  zipP
   (zipP
    (zipP
     (zipP
      (zipP
       (Pipes.map fst
        >-> Pipes.map (("next: " <>) . pack . show)
        >-> Pipes.drop 1)
       (Pipes.map fst
        >-> addCountP
        >-> Pipes.map (\(x,y) -> XY (fromIntegral x) y)
        >-> Pipes.drop 1)
      )
      (Pipes.map fst
       >-> addCountP
       >-> Pipes.map (\(x,y) -> XY (fromIntegral x) y)
       >-> Pipes.drop 1)
     )
     (L.purely Pipes.scan (trimmedIncHist2D (c^.mini,c^.maxi) (c^.heatGrain) (c^.eps) (c^.decayRate))
      >-> Pipes.map hist2Rect >-> Pipes.drop 1)
     )
    (Pipes.map fst
     >-> Pipes.map (max (c^.mini) . min ((c^.maxi)-(c^.eps)))
     >-> L.purely Pipes.scan (incHist (binD (c^.mini) (c^.maxBins) (c^.maxi)) (c^.decayRate))
     >-> Pipes.map hist2xyz
     >-> Pipes.drop 1)
    )
   (Pipes.map fst
    >-> L.purely Pipes.scan (incAdaptiveHist (c^.thresh) (c^.adaptGrain) (c^.maxBins) (c^.decayRate))
    >-> Pipes.map hist2xyz
    >-> Pipes.drop 1
    >-> Pipes.map normYOnZ)
   >-> Pipes.map (\(((((a, b), c), d), e), f) -> Combo a b c d e f)

comboPage :: Html ()
comboPage =
  Chart.Render.render
   (chartCss "chart")
   [ ucChart . cName .~ "charttext"
   $ ucChart . cCss .~ (Css.element "#charttext" ? Css.clear Css.clearLeft)
   $ defaultChart [eUpdateExpr .~ r "d.comboText" $ ChartText.testElement]
   , ucChart . cName .~ "chartline" 
   $ defaultChart [eUpdateExpr .~ r "d.comboLine" $ Line.testElement]
   , ucChart . cName .~ "chartscroll" 
   $ defaultChart [eUpdateExpr .~ r "d.comboScroll" $ Line.testElementScroll]
   , ucChart . cName .~ "chartheatmap" 
   $ ucChart . cDims .~
     [ Dim "x" Nothing Nothing 
     , Dim "y" Nothing Nothing
     , Dim "z" Nothing Nothing
     ]
   $ defaultChart 
      [ eUpdateExpr .~ r "d.comboHeatmap"
      $ eCreate .~ Heatmap.heatmapCreate
          (rectZero (def^.mini) (def^.maxi) (def^.heatGrain))
      $ Heatmap.testElement
      ]
   , ucChart . cName .~ "charthist" 
   $ defaultChart [eUpdateExpr .~ r "d.comboHist" $ BarW.testElement]
   , ucChart . cName .~ "charthistadapt" 
   $ ucChart . cDims .~
     [ Dim "x" Nothing Nothing 
     , Dim "y" (Just 0) Nothing
     ]
   $ defaultChart [eUpdateExpr .~ r "d.comboHistAdapt" $ BarW.testElementAdapt]
   ]
 
testPlay :: IO ()
testPlay = void $ runPlay (comboProducer def) comboPage def

-- pipes helpers
addCountP :: (Monad m) => Pipe a (Int,a) m ()
addCountP = flip S.evalStateT 0 $ distribute $ do
    lift $ S.put 0
    forever $ do
        s <- await
        lift $ S.modify (+1)
        y <- lift S.get
        yield (y,s)

dupP :: (Monad m) => Pipe a (a,a) m r
dupP = Pipes.map dup
  where
    dup a = (a,a)

splitP :: (Monad m) => Pipe (a,a) a m r
splitP = forever $ do
  (a1,a2) <- await
  yield a1
  yield a2

firstP :: (Monad m) => Pipe b c m () -> Pipe (b,d) (c,d) m ()
firstP p = flip S.evalStateT undefined $ distribute $ firstSaveP >-> unsafeHoist lift p >-> firstRestoreP

firstSaveP :: (Monad m) => Pipe (b,d) b (S.StateT d m) ()
firstSaveP = forever $ do
  (b,d) <- await
  lift $ S.put d
  yield b

firstRestoreP :: (Monad m) => Pipe c (c,d) (S.StateT d m) ()
firstRestoreP = forever $ do
  c <- await
  d <- lift S.get
  yield (c,d)

secondP :: (Monad m) => Pipe c d m () -> Pipe (b,c) (b,d) m ()
secondP p = Pipes.map swap >-> firstP p >-> Pipes.map swap

zipP ::  (Monad m) => Pipe b c m () -> Pipe b c' m () -> Pipe b (c,c') m ()
zipP p1 p2 = dupP >-> firstP p1 >-> secondP p2
