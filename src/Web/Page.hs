{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Web.Page where

import Lucid.Css as Css
import Lucid.Svg
import Lucid.Js as Js
import           Control.Lens
import           Data.Monoid
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Lucid
import Control.Applicative

data Page =
    Page
    { _pageLibsCss :: [String]
    , _pageLibsJs :: [String]
    , _pageCss :: Css
    , _pageJsGlobal :: JStat
    , _pageJsOnLoad :: JStat
    , _pageHtmlHeader :: Html ()
    , _pageHtmlBody :: Html ()
    } deriving (Show)

makeLenses ''Page

instance Monoid Page where
    mempty = Page [] [] mempty mempty mempty mempty mempty
    mappend p0 p1 =
        Page
        (p0^.pageLibsCss <> p1^.pageLibsCss)
        (p0^.pageLibsJs <> p1^.pageLibsJs)
        (p0^.pageCss        <> p1^.pageCss)
        (p0^.pageJsGlobal   <> p1^.pageJsGlobal)
        (p0^.pageJsOnLoad   <> p1^.pageJsOnLoad)
        (p0^.pageHtmlHeader <> p1^.pageHtmlHeader)
        (p0^.pageHtmlBody   <> p1^.pageHtmlBody)

chartLibs :: Page
chartLibs =
    Page
    ["http://netdna.bootstrapcdn.com/font-awesome/3.2.1/css/font-awesome.css"] 
    [ "http://d3js.org/d3.v3.min.js"
    , "http://code.jquery.com/jquery-1.6.3.min.js"
    ]
    mempty
    mempty
    mempty
    mempty
    mempty

renderLibsCss :: [Text] -> Html ()
renderLibsCss xs =
    mconcat $
    ((\x -> link_ [x,rel_ "stylesheet"]) . href_) <$> xs

renderLibsJs :: [Text] -> Html ()
renderLibsJs xs = mconcat ((\x -> with (script_ mempty) [src_ x]) <$> xs)

meta :: Html ()
meta = meta_ [charset_ "utf-8"]

renderPage :: Page -> Html ()
renderPage p =
    doctypehtml_ $
    head_
    (  Web.Page.meta
    <> renderLibsCss (pack <$> (p^.pageLibsCss))
    <> renderLibsJs (pack <$> p^.pageLibsJs)
    <> script_ mempty (Js.render (p^.pageJsGlobal <> Js.onLoad (p^.pageJsOnLoad)))
    <> style_ (Lazy.toStrict $ Css.render (p^.pageCss))
    <> p^.pageHtmlHeader) <>
    body_ (p^.pageHtmlBody)

renderSvg :: Page -> Html ()
renderSvg p =
    doctypesvg_ <> Lucid.Svg.svg_ (defs_
                         (  renderLibsJsSvg (pack <$> p^.pageLibsJs)
                         <> renderLibsCssSvg (pack <$> p^.pageLibsCss)
                         <> renderCssSvg (p^.pageCss)
                         <> script_ mempty (Js.render (p^.pageJsGlobal <> Js.onLoad (p^.pageJsOnLoad)))
                         <> (p^.pageHtmlBody)))

pageJsToFile :: FilePath -> Page -> IO ()
pageJsToFile file p = Js.jsToFile file (p^.pageJsGlobal <> Js.onLoad (p^.pageJsOnLoad))

