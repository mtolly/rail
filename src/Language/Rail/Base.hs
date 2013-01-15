{-# LANGUAGE DeriveDataTypeable #-}
module Language.Rail.Base where

import Data.Data (Data, Typeable)

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
  deriving (Eq, Ord, Show, Read)
