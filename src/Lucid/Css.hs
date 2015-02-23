{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Lucid.Css
  ( Margins(..)
  , fill
  , stroke
  , strokeWidth
  , crispEdges
  , Lucid.Css.optimizeSpeed
  , Lucid.Css.geometricPrecision
  , shapeRendering
  , cssDefaultColor
  , cssMargins
  , module Clay
  , pack
  ) where

import           Clay hiding (optimizeSpeed, geometricPrecision, PlayState)
import           Clay.Stylesheet (key)
import           Language.Javascript.JMacro (ToJExpr(..))
import           Control.Lens hiding (element)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Text (Text, pack)
import           Data.Text.Lazy (unpack)

instance Monoid Css where
    mempty = return ()
    mappend a b = a >> b

instance Show Css where
    show = unpack . render

{- CSS -}
data Margins = 
    Margins
    { _mTop :: Integer
    , _mRight :: Integer
    , _mBottom :: Integer
    , _mLeft :: Integer
    } deriving (Show, Eq)

makeLenses ''Margins

instance ToJExpr Margins where
    toJExpr (Margins t r b l) =
        toJExpr
        (Map.fromList
         [ ("top", t)
         , ("right", r)
         , ("bottom", b)
         , ("left", l)
         ] :: Map.Map String Integer)

-- SVG css
fill :: Color -> Css
fill = key "fill"

stroke :: Color -> Css
stroke = key "stroke"

strokeWidth :: Size Abs -> Css
strokeWidth = key "stroke-width"

newtype ShapeRendering = ShapeRendering Value
  deriving (Inherit, Auto, Val)

crispEdges, optimizeSpeed, geometricPrecision :: ShapeRendering

crispEdges = ShapeRendering "crispEdges"
optimizeSpeed = ShapeRendering "optimizeSpeed"
geometricPrecision = ShapeRendering "geometricPrecision"

shapeRendering :: ShapeRendering -> Css
shapeRendering = key "shape-rendering"

-- css constructors
cssDefaultColor :: Text -> Css
cssDefaultColor name =
      body ? element name ?
            Clay.color Clay.steelblue

cssMargins :: Text -> Margins -> Css
cssMargins name margins =
      body ? element name ? do
            Clay.marginTop (px (margins^.mTop))
            Clay.marginLeft (px (margins^.mLeft))
            Clay.marginBottom (px (margins^.mBottom))
            Clay.marginRight (px (margins^.mRight))
