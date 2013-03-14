{-# LANGUAGE DeriveDataTypeable #-}
module Language.Rail.Base
( Command(..)
, Val(..)
, Result(..)
, systemVars
, ToVal(..)
) where

import Data.Data (Data, Typeable)
import Data.ControlFlow
import Data.List (nub)
import qualified Data.Map as Map
import Data.Char (isSpace)

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
  = Str String (Maybe Integer)
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

-- | General class for things that can be converted to Rail values.
-- Uses the same list hack as the 'Show' class, in order to treat Strings
-- differently without using any language extensions.
--
-- Minimal definition: toVal.
class ToVal a where
  toVal :: a -> Val
  listToVal :: [a] -> Val
  listToVal = foldr (\x l -> Pair (toVal x) l) Nil

instance (ToVal a) => ToVal [a] where
  toVal = listToVal

instance ToVal Char where
  toVal c = let s = [c] in Str s $ readMaybe s
  listToVal s = Str s $ readMaybe s

instance ToVal Int where
  toVal i = Str (show i) $ Just $ fromIntegral i

instance ToVal Integer where
  toVal i = Str (show i) $ Just i

instance ToVal Bool where
  toVal b = toVal $ fromEnum b

instance ToVal Val where
  toVal = id

instance (ToVal a, ToVal b) => ToVal (a, b) where
  toVal (x, y) = Pair (toVal x) (toVal y)

instance (ToVal a) => ToVal (Maybe a) where
  toVal = maybe Nil toVal

instance ToVal () where
  toVal () = Nil

-- | Equivalent to 'read', except it returns Nothing on read error.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(n, sp)] | all isSpace sp -> Just n
  _ -> Nothing
