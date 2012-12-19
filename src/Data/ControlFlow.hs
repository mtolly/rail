{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | A data structure for modeling control flow graphs. Basic blocks can be
-- constructed separately, with arbitrary IDs attached to each one; you can
-- then generate a single circular structure for the entire graph, taking up
-- a finite amount of space.
module Data.ControlFlow where

import Data.Void (Void)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State (execState, gets, modify)
import Control.Monad (unless)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- | A partial control flow graph, with nodes of type 'a'. Each leaf of the tree
-- ends in a value of type 'e', and the graph can also contain arbitrary labels
-- as continuations, of type 'c'.
data Go c e a
  = a        :>> Go c e a -- ^ Sequence
  | Go c e a :|| Go c e a -- ^ Branch
  | Continue c
  | End e
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

infixr 1 :>>
infix  1 :||

-- | A control flow graph without continuation labels.
type Flow e a = Go Void e a

-- | A complete control flow graph in the form of a starting block, and a
-- mapping from labels to blocks.
data System c e a = System
  { systemStart :: Go c e a
  , systemPaths :: Map.Map c (Go c e a)
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Returns the set of all labels which are reachable from the start path.
usedPaths :: Ord c => System c e a -> Set.Set c
usedPaths sys = go (systemStart sys) `execState` Set.empty where
  go g = case g of
    _ :>> next -> go next
    x :|| y -> go x >> go y
    Continue label -> gets (Set.member label) >>= \b -> unless b $ do
      modify $ Set.insert label
      maybe missingPath go $ Map.lookup label paths
    End _ -> return ()
  missingPath = error "usedPaths: missing continue"
  paths = systemPaths sys

-- | Removes all unused continuations from the map.
cleanPaths :: Ord c => System c e a -> System c e a
cleanPaths sys = let
  used = Map.fromDistinctAscList
    [(k, undefined) | k <- Set.toAscList $ usedPaths sys]
  in sys { systemPaths = systemPaths sys `Map.intersection` used }

-- | Given a start point and a mapping from continutation labels to code
-- chunks, creates a single structure embedding the entire control flow.
-- The structure may be circular, but it will only take up a finite amount
-- of memory, and all continuation paths will only be computed once.
flow :: (Ord c) => System c e a -> Flow e a
flow sys = let
  flows = fmap toFlow $ systemPaths sys
  getFlow label = fromMaybe (error "flow: missing continue") $
    Map.lookup label flows
  toFlow g = case g of
    x :>> next -> x :>> toFlow next
    x :|| y -> toFlow x :|| toFlow y
    Continue label -> getFlow label
    End x -> End x
  in toFlow $ systemStart sys
