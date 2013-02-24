{-# LANGUAGE DeriveDataTypeable #-}
module Language.Rail.Base
( Command(..)
, Val(..)
, Result(..)
, systemVars
) where

import Data.Data (Data, Typeable)
import Data.ControlFlow
import Data.List (nub)
import qualified Data.Map as Map

data Command
  = EOF
  | Input
  | Output
  | Underflow
  | Type
  | Push String -- ^ read a var, push to stack
  | Pop String -- ^ pop from stack, store in var
  | Call String -- ^ function call
  | Add
  | Div
  | Mult
  | Rem
  | Sub
  | Val Val -- ^ push a constant value
  | Cut
  | Append
  | Size
  | Cons
  | Uncons
  | Greater
  | Equal
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Val
  = Str String
  | Nil
  | Pair Val Val
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Result
  = Return
  | Boom
  | Internal String
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Find all the local variables used in a function.
systemVars :: System c () Result Command -> [String]
systemVars (System st ps) = let
  pathVars :: Path c () Result Command -> [String]
  pathVars g = case g of
    Push v :>> x  -> v : pathVars x
    Pop  v :>> x  -> v : pathVars x
    _      :>> x  -> pathVars x
    Branch () x y -> pathVars x ++ pathVars y
    _             -> []
  in nub $ concatMap pathVars $ st : Map.elems ps
