{-# LANGUAGE GADTs #-}
-- | Optimize Rail functions so sequences of stack operations are replaced with
-- expressions, which can translate directly to high-level language expressions.
module Language.Rail.Expression where

import Language.Rail.Base (Command(), Val())
import qualified Language.Rail.Base as R

-- | An expression is a side-effect-free value that can be converted to a linear
-- sequence of stack operations. Its value depends only on local variables at
-- the time of evaluation; its only change to the stack is to place its single
-- return value on top.
data Expr a where
  Val :: Val -> Expr Val
  Var :: String -> Expr Val
  GetStr :: Expr Val -> Expr String
  PutStr :: Expr String -> Expr Val
  ReadInt :: Expr String -> Expr Integer
  ShowInt :: Expr Integer -> Expr String
  Add  :: Expr Integer -> Expr Integer -> Expr Integer
  Sub  :: Expr Integer -> Expr Integer -> Expr Integer
  Mult :: Expr Integer -> Expr Integer -> Expr Integer
  Div  :: Expr Integer -> Expr Integer -> Expr Integer
  Rem  :: Expr Integer -> Expr Integer -> Expr Integer

render :: Expr a -> [Command]
render e = case e of
  Val v -> [R.Val v]
  Var s -> [R.Push s]
  GetStr x -> render x
  PutStr x -> render x
  ReadInt x -> render x
  ShowInt x -> render x
  Add  x y -> render x ++ render y ++ [R.Add]
  Sub  x y -> render x ++ render y ++ [R.Sub]
  Mult x y -> render x ++ render y ++ [R.Mult]
  Div  x y -> render x ++ render y ++ [R.Div]
  Rem  x y -> render x ++ render y ++ [R.Rem]
