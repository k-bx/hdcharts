{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Javascript.JMacro.D3Expr where

import Language.Javascript.JMacro
import Data.Monoid

data D3Expr = D3Expr String (Maybe [JExpr]) deriving (Show)

instance ToJExpr D3Expr where
  toJExpr (D3Expr a Nothing) = ValExpr (JVar (StrI a))
  toJExpr (D3Expr a (Just es)) = ApplExpr (ValExpr (JVar (StrI a))) es

instance ToJExpr [D3Expr] where
  toJExpr [] = error "no mempty for a JExpr"
  toJExpr [x] = toJExpr x
  toJExpr (x:xs) =
    case x of
      (D3Expr a Nothing) -> SelExpr (toJExpr xs) (StrI a)
      (D3Expr a (Just es)) -> ApplExpr (SelExpr (toJExpr xs) (StrI a)) es

instance ToStat [D3Expr] where
  toStat = toStat . toJExpr

toD3Expr :: JExpr -> [D3Expr]
toD3Expr (ValExpr (JVar (StrI a))) = [D3Expr a Nothing]
toD3Expr (ApplExpr (ValExpr (JVar (StrI a))) es) = [D3Expr a (Just es)]
toD3Expr (SelExpr x (StrI a)) = [D3Expr a Nothing] <> toD3Expr x
toD3Expr (ApplExpr (SelExpr x (StrI a)) es) =
  [D3Expr a (Just es)] <> toD3Expr x
toD3Expr x = error (show x)

d3 :: String -> [D3Expr]
d3 e = toD3Expr [jmacroE|`(jsv e)`|] 

ex1'' :: JExpr
ex1'' =
  [jmacroE|
   d3.select("body").
   style("color", "black").
   style("background-color", "white")
  |]

ex1' :: JStat
ex1' =
  [jmacro|
   d3.select("body").
   style("color", "black").
   style("background-color", "white");
  |]

testEx1 :: Bool
testEx1 = ex1' == [jmacro|`(ex1'')`;|]

line1 :: JExpr
line1 =
  [jmacroE|
   d3.select("body")
  |]

line2 :: JExpr
line2 =
  [jmacroE|
   style("color", "black")
  |]

line3 :: JExpr
line3 =
  [jmacroE|
   style("background-color", "white")
  |]

(<.>) :: [D3Expr] -> [D3Expr] -> [D3Expr]
(<.>) x y = y<>x

exD3 :: JStat
exD3 =
  toStat $
  toJExpr $
  toD3Expr line1 <.>
  toD3Expr line2 <.>
  toD3Expr line3

testD3 :: Bool
testD3 = ex1' == exD3
