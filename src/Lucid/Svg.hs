{-# LANGUAGE OverloadedStrings #-}

module Lucid.Svg where

import Control.Applicative
import Data.Monoid
import Data.Text (Text, pack)
import Lucid
import Lucid.Base
import Lucid.Css (Css, render)

{- | an svg element
-}
doctypesvg_  :: Monad m => HtmlT m ()
doctypesvg_ = makeElementNoEnd "?xml version='1.0' encoding='UTF-8'?><!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN\'\n'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'"

xml_  :: Monad m => HtmlT m ()
xml_ = makeElementNoEnd "?xml version=\"1.0\" standalone=\"no\"?"

svg_  :: Monad m => HtmlT m () -> HtmlT m ()
svg_ = with (makeElement "svg")
       [ makeAttribute "xmlns" "http://www.w3.org/2000/svg"
       , makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
       ]

linkSvg_  :: Monad m => HtmlT m () -> HtmlT m ()
linkSvg_ = makeElement "link"

defs_ ::
  Monad m => HtmlT m a -> HtmlT m a
defs_ = makeElement "defs"

renderLibsJsSvg :: [Text] -> Html ()
renderLibsJsSvg xs = mconcat $ libJsSvg <$> xs

libJsSvg :: Text -> Html ()
libJsSvg x =
  with (script_ mempty)
      [ href_ x
      , rel_ "stylesheet"
      , type_ (pack "text/css")
      , makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
      ]

renderLibsCssSvg :: [Text] -> Html ()
renderLibsCssSvg xs =
    (mconcat $
    (with linkSvg_ .
     (\x ->
      [ href_ x
      , rel_ "stylesheet"
      , type_ (pack "text/css")
      , makeAttribute "xmlns" "http://www.w3.org/1999/xhtml"
      ])) <$> xs) mempty

renderCssSvg :: Css -> Html ()
renderCssSvg css =
    style_ 
      [ type_ (pack "text/css")
      ] (render css)
