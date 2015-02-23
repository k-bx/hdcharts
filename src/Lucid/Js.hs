{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- some js combinators and helpers
module Lucid.Js
  ( module Lucid.Js
  , module Language.Javascript.JMacro
  , module Language.Javascript.JMacro.D3Expr
  ) where

import Blaze.ByteString.Builder
import Control.Applicative
import Data.ByteString.Lazy as Lazy
import Data.Monoid
import Data.String
import GHC.Char
import Language.JavaScript.Parser
import Language.Javascript.JMacro
import Language.Javascript.JMacro.D3Expr

($.) :: JExpr -> String -> JExpr
x $. y = SelExpr x (StrI y)

($$) :: (ToJExpr a, ToJExpr b) => a -> b -> JExpr
x $$  y = ApplExpr (toJExpr x) (toJExprList y)

($$$) :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
x $$$  y = ApplStat (toJExpr x) (toJExprList y)

instance IsString JExpr where
  fromString x = r x

r :: String -> JExpr
r = ValExpr . JVar . StrI

lit :: String -> JExpr
lit s = (ValExpr . JStr) s

var :: String -> JStat
var sym = DeclStat (StrI sym) Nothing

declObj :: String -> JStat
declObj s =
       var s
    <> (r s =: r "{}")

-- | wraps statements in a function and returns parameter
wrapF :: String -> JStat -> JExpr
wrapF c stats = ValExpr (JFunc [StrI c] (stats <> ReturnStat (r c)))

wrapF2 :: String -> String -> JStat -> JExpr
wrapF2 c d stats  = ValExpr (JFunc [StrI c,StrI d] (stats <> ReturnStat (r c)))

wrapF0 :: String -> JStat -> JExpr
wrapF0 c stats = ValExpr $ JFunc [StrI c] stats

render :: JStat -> String
render = Prelude.map (chr . fromIntegral) . Lazy.unpack . toLazyByteString . renderJS . readJs . show . renderJs

jsToFile :: FilePath -> JStat -> IO ()
jsToFile file js = Prelude.writeFile file (render js)


-- chain
chain :: ToJExpr a => String -> [a] -> JExpr -> JExpr
chain x args y = ApplExpr (SelExpr y (StrI x)) (fmap toJExpr args)

ch :: JExpr -> JExpr -> JExpr
ch (ApplExpr (ValExpr (JVar (StrI x))) args) = chain x args
-- ch (ApplExpr (SelExpr y (StrI x)) args) = (chain x args) y
ch e = error (show e)

-- fixme: sort through these
fun0 :: JStat -> JExpr
fun0 es = ValExpr (JFunc [] es)

fun :: [String] -> JStat -> JExpr
fun args es = ValExpr (JFunc (StrI <$> args) es) 

switch :: JExpr -> [(JExpr,JStat)] -> JStat -> JStat
switch x cases def = SwitchStat (toJExpr x) ((\(b,s) -> (toJExpr b,s)) <$> cases) def

onLoad :: JStat -> JStat
onLoad es = r "window.onload" =: fun0 es

jq :: String -> JExpr
jq s = r "$" $$ lit s

infixl 2 =:
(=:) :: ToJExpr a => JExpr -> a -> JStat
x =:  y = AssignStat x (toJExpr y)

null :: JExpr
null  = jsv "null" -- fixme

if' :: (ToJExpr a, ToStat b) => a -> b -> JStat
if' x y       = IfStat (toJExpr x) (toStat y) (BlockStat [])

ifElse :: (ToJExpr a, ToStat b, ToStat c) => a -> b -> c -> JStat
ifElse x y z = IfStat (toJExpr x) (toStat y) (toStat z)

while :: ToJExpr a => a -> JStat -> JStat
while x y = WhileStat False (toJExpr x) y

return :: ToJExpr a => a -> JStat
return x = ReturnStat (toJExpr x)

toJExprList :: ToJExpr a => a -> [JExpr]
toJExprList x = case toJExpr x of
                  (ValExpr (JList l)) -> l
                  x' -> [x']

jstr :: String -> JExpr
jstr = ValExpr . JStr

-- chain example

d3' :: JExpr
d3' =             [jmacroE| d3 |]
selBody :: JExpr -> JExpr
selBody =      ch [jmacroE| select("body")|]
styleBlack :: JExpr -> JExpr
styleBlack =   ch [jmacroE| style("color", "black")|]
styleBgWhite :: JExpr -> JExpr
styleBgWhite = ch [jmacroE| style("background-color", "white")|]
ex1 :: JExpr
ex1 = styleBlack . styleBgWhite . selBody $ d3'
