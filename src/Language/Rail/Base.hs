{-# LANGUAGE DeriveDataTypeable #-}
-- | The core datatypes for the Rail language.
module Language.Rail.Base
( Command(..)
, Val(..)
, Result(..)
, systemVars
, Value(..)
) where

import Data.Data (Data, Typeable)
import Data.ControlFlow
import Data.List (nub)
import qualified Data.Map as Map
import Data.Char (isSpace)
import Control.Applicative (liftA2)

-- | An instruction in a Rail program.
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

-- | A value stored on the Rail stack.
data Val
  = Str String (Maybe Integer)
  | Nil
  | Pair Val Val
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | The end result of a Rail function.
data Result
  = Return -- ^ a normal function return
  | Boom -- ^ pop a string value off the stack, which is an error message
  | Internal String -- ^ the result of a parse error or other static error
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

-- | General class for things that can be converted to/from Rail values.
-- Uses the same list hack as the 'Show' and 'Read' classes, in order to treat
-- Strings differently without using any language extensions.
--
-- Minimal definition: toVal and fromVal.
class Value a where
  toVal       :: a   -> Val
  listToVal   :: [a] -> Val
  fromVal     :: Val -> Maybe a
  listFromVal :: Val -> Maybe [a]
  
  listToVal              = foldr (\x l -> Pair (toVal x) l) Nil
  listFromVal Nil        = Just []
  listFromVal (Pair x y) = liftA2 (:) (fromVal x) (listFromVal y)
  listFromVal _          = Nothing

instance (Value a) => Value [a] where
  toVal   = listToVal
  fromVal = listFromVal

instance Value Char where
  toVal       c           = let s = [c] in Str s $ readMaybe s
  listToVal   s           = Str s $ readMaybe s
  fromVal     (Str [c] _) = Just c
  fromVal     _           = Nothing
  listFromVal (Str s _)   = Just s
  listFromVal _           = Nothing

instance Value Int where
  toVal   i                = Str (show i) $ Just $ fromIntegral i
  fromVal (Str _ (Just i)) = Just $ fromIntegral i
  fromVal _                = Nothing

instance Value Integer where
  toVal   i                = Str (show i) $ Just i
  fromVal (Str _ (Just i)) = Just i
  fromVal _                = Nothing

instance Value Bool where
  toVal   b                = toVal $ fromEnum b
  fromVal (Str _ (Just 0)) = Just False
  fromVal (Str _ (Just 1)) = Just True
  fromVal _                = Nothing

instance Value Val where
  toVal   = id
  fromVal = Just

instance (Value a, Value b) => Value (a, b) where
  toVal   (x, y)     = Pair (toVal x) (toVal y)
  fromVal (Pair x y) = liftA2 (,) (fromVal x) (fromVal y)
  fromVal _          = Nothing

instance (Value a) => Value (Maybe a) where
  toVal       = maybe Nil toVal
  fromVal Nil = Just Nothing
  fromVal x   = fmap Just $ fromVal x

instance Value () where
  toVal   ()  = Nil
  fromVal Nil = Just ()
  fromVal _   = Nothing

-- | Equivalent to 'read', except it returns Nothing on read error.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(n, sp)] | all isSpace sp -> Just n
  _ -> Nothing
