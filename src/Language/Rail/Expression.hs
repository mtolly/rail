{-# LANGUAGE GADTs #-}
-- | Optimize Rail functions so sequences of stack operations are replaced with
-- expressions, which can translate directly to high-level language expressions.
module Language.Rail.Expression where

import Language.Rail.Base (Command(), Val(), Result())
import qualified Language.Rail.Base as R
import Data.Char (isSpace)
import Data.ControlFlow
import Data.List (genericLength)

-- | An expression is a side-effect-free value that can be converted to a linear
-- sequence of stack operations. Its value depends only on local variables at
-- the time of evaluation; its only change to the stack is to place its single
-- return value on top.
data Expr a where
  Int :: Integer -> Expr Integer
  Str :: String -> Expr String
  Bool :: Bool -> Expr Bool
  Val :: Val -> Expr Val
  Var :: String -> Expr Val
  GetStr :: Expr Val -> Expr String
  PutStr :: Expr String -> Expr Val
  ReadInt :: Expr String -> Expr Integer
  ShowInt :: Expr Integer -> Expr String
  ToBool :: Expr Integer -> Expr Bool
  FromBool :: Expr Bool -> Expr Integer
  Add  :: Expr Integer -> Expr Integer -> Expr Integer
  Sub  :: Expr Integer -> Expr Integer -> Expr Integer
  Mult :: Expr Integer -> Expr Integer -> Expr Integer
  Div  :: Expr Integer -> Expr Integer -> Expr Integer
  Rem  :: Expr Integer -> Expr Integer -> Expr Integer
  Append :: Expr String -> Expr String -> Expr String
  Size :: Expr String -> Expr Integer
  Greater :: Expr Integer -> Expr Integer -> Expr Bool
  TypeError :: String -> String -> Expr a
  -- ^ Params are expected and actual types of a subexpression

-- | Evaluates an expression via Rail commands, and then proceeds to the given
-- path.
render :: Expr a -> Path c () Result Command -> Path c () Result Command
render e cont = case e of
  Int i -> R.Val (R.Str $ show i) :>> cont
  Str s -> R.Val (R.Str s) :>> cont
  Bool b -> R.Val (R.Str $ if b then "1" else "0") :>> cont
  Val v -> R.Val v :>> cont
  Var s -> R.Push s :>> cont
  GetStr x -> render x cont
  PutStr x -> render x cont
  ReadInt x -> render x cont
  ShowInt x -> render x cont
  ToBool x -> render x cont
  FromBool x -> render x cont
  Add  x y -> render x $ render y $ R.Add  :>> cont
  Sub  x y -> render x $ render y $ R.Sub  :>> cont
  Mult x y -> render x $ render y $ R.Mult :>> cont
  Div  x y -> render x $ render y $ R.Div  :>> cont
  Rem  x y -> render x $ render y $ R.Rem  :>> cont
  Append x y -> render x $ render y $ R.Append :>> cont
  Size x -> render x $ R.Size :>> cont
  Greater x y -> render x $ render y $ R.Greater :>> cont
  TypeError ex act -> End $ R.Internal $
    "Type error in expression: expected " ++ ex ++ ", got " ++ act

simplify :: Expr a -> Expr a
simplify e = case e of
  GetStr v -> case simplify v of
    PutStr s -> s
    Val (R.Str s) -> Str s
    simp -> GetStr simp
  PutStr s -> case simplify s of
    Str a -> Val (R.Str a)
    GetStr v -> v
    simp -> PutStr simp
  ReadInt s -> case simplify s of
    Str a -> case readMaybe a of
      Just i -> Int i
      Nothing -> TypeError "int" "non-int string"
    ShowInt i -> i
    simp -> ReadInt simp
  ShowInt i -> case simplify i of
    ReadInt s -> s
    Int a -> Str $ show a
    simp -> ShowInt simp
  ToBool i -> case simplify i of
    Int 0 -> Bool False
    Int 1 -> Bool True
    Int _ -> TypeError "bool" "non-bool int"
    FromBool b -> b
    simp -> ToBool simp
  FromBool b -> case simplify b of
    Bool a -> Int $ if a then 1 else 0
    ToBool i -> i
    simp -> FromBool simp
  Add x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Int $ a + b
    (sx, sy) -> Add sx sy
  Sub x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Int $ a - b
    (sx, sy) -> Sub sx sy
  Mult x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Int $ a * b
    (sx, sy) -> Mult sx sy
  Div x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Int $ div a b
    (sx, sy) -> Div sx sy
  Rem x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Int $ mod a b
    (sx, sy) -> Rem sx sy
  Append x y -> case (simplify x, simplify y) of
    (Str a, Str b) -> Str $ a ++ b
    (sx, sy) -> Append sx sy
  Size s -> case simplify s of
    Str a -> Int $ genericLength a
    Append a b -> Add (Size a) (Size b)
    simp -> Size simp
  Greater x y -> case (simplify x, simplify y) of
    (Int a, Int b) -> Bool $ a > b
    (sx, sy) -> Greater sx sy
  _ -> e

-- | Equivalent to 'read', except it returns Nothing on read error.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(n, sp)] | all isSpace sp -> Just n
  _ -> Nothing
