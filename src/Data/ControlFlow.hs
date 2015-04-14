{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP                #-}
-- | A data structure for modeling control flow graphs. Basic blocks can be
-- constructed separately, with arbitrary IDs attached to each one; you can
-- then generate a single circular structure for the entire graph, taking up
-- a finite amount of space.
module Data.ControlFlow
( Path(..)
, Flow
, System(..)
, usedPaths
, cleanPaths
, mapPaths
, simplifyPaths
, flow
, mapContinues
, numberPaths
, cleanContinues
, continues
) where

import Data.Void (Void)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State (execState, gets, modify)
import Control.Monad (unless)
#if __GLASGOW_HASKELL__ < 710
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import Data.List (sort, group)
import Data.Data (Data, Typeable)

-- | A partial control flow graph, with nodes of type @a@, branch nodes of type
-- @b@, continuation labels of type @c@, and leaf nodes of type @e@.
data Path c b e a
  = a :>> Path c b e a
  | Branch b (Path c b e a) (Path c b e a)
  | Continue c
  | End e
  deriving
    ( Eq, Ord, Show, Read
    , Functor, Foldable, Traversable
    , Data, Typeable )

infixr 1 :>>

-- | A control flow graph without continuation labels.
type Flow = Path Void

-- | A complete control flow graph in the form of a starting block, and a
-- mapping from labels to blocks.
data System c b e a = System
  { systemStart :: Path c b e a
  , systemPaths :: Map.Map c (Path c b e a)
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

-- | Returns the set of all labels which are reachable from the start path.
usedPaths :: (Ord c) => System c b e a -> Set.Set c
usedPaths sys = go (systemStart sys) `execState` Set.empty where
  go g = case g of
    _ :>> x -> go x
    Branch _ x y -> go x >> go y
    Continue label -> gets (Set.member label) >>= \b -> unless b $ do
      modify $ Set.insert label
      maybe missingPath go $ Map.lookup label paths
    End _ -> return ()
  missingPath = error "usedPaths: missing continue"
  paths = systemPaths sys

-- | Removes all unused continuations from the map.
cleanPaths :: (Ord c) => System c b e a -> System c b e a
cleanPaths sys = let
  used = Map.fromDistinctAscList
    [(k, undefined) | k <- Set.toAscList $ usedPaths sys]
  in sys { systemPaths = systemPaths sys `Map.intersection` used }

-- | The list of all labels directly continued to by the given path. May contain
-- duplicates, if there are two continues to the same label.
continues :: (Ord c) => Path c b e a -> [c]
continues g = case g of
  _ :>> x      -> continues x
  Branch _ x y -> continues x ++ continues y
  Continue c   -> [c]
  End _        -> []

-- | @unroll l x g@ finds all places where @g@ continues to the label @l@, and
-- replaces them with the path @x@.
unroll :: (Eq c) => c -> Path c b e a -> Path c b e a -> Path c b e a
unroll cfrom gto = go where
  go g = case g of
    v :>> x                 -> v    :>> go x
    Branch b x y            -> Branch b (go x) (go y)
    Continue c | c == cfrom -> gto
    _                       -> g

-- | Applies a function to every path inside a system.
mapPaths
  :: (Path c b e a -> Path c b' e' a') -> System c b e a -> System c b' e' a'
mapPaths f sys = System
  { systemStart = f $ systemStart sys
  , systemPaths = fmap f $ systemPaths sys }

-- | Like 'unroll', but applied to a network of paths.
unrollSystem :: (Ord c) => c -> System c b e a -> System c b e a
unrollSystem c sys = case Map.lookup c $ systemPaths sys of
  Nothing -> sys
  Just path -> mapPaths (unroll c path) $
    sys { systemPaths = Map.delete c $ systemPaths sys }

-- | Sorts a list, and then returns only elements that appear just once.
uniqueElems :: (Ord a) => [a] -> [a]
uniqueElems xs = [ x | [x] <- group $ sort xs ]

-- | A list of all continuation labels that only appear once.
usedOnce :: (Ord c) => System c b e a -> [c]
usedOnce sys = uniqueElems $ concatMap continues $
  systemStart sys : Map.elems (systemPaths sys)

-- | For each path which is only referenced in one location, removes the path
-- and pastes its contents into the place it was referenced.
simplifyPaths :: (Ord c) => System c b e a -> System c b e a
simplifyPaths sys = foldr unrollSystem sys $ usedOnce sys

-- | Given a start point and a mapping from continutation labels to code
-- chunks, creates a single structure embedding the entire control flow.
-- The structure may be circular, but it will only take up a finite amount
-- of memory, and all continuation paths will only be computed once.
flow :: (Ord c) => System c b e a -> Flow b e a
flow sys = let
  flows = fmap toFlow $ systemPaths sys
  getFlow label = fromMaybe (error "flow: missing continue") $
    Map.lookup label flows
  toFlow g = case g of
    v :>> x      -> v :>> toFlow x
    Branch b x y -> Branch b (toFlow x) (toFlow y)
    Continue c   -> getFlow c
    End e        -> End e
  in toFlow $ systemStart sys

-- | Applies a function to every continuation label in a path.
mapContinues :: (c -> c') -> Path c b e a -> Path c' b e a
mapContinues f g = case g of
  Continue c   -> Continue $ f c
  x :>> xs     -> x :>> mapContinues f xs
  Branch b x y -> Branch b (mapContinues f x) (mapContinues f y)
  End e        -> End e

-- | Replaces continuation labels with numbers starting from 0.
numberPaths :: (Eq c) => System c b e a -> System Int b e a
numberPaths (System st ps) = let
  contToInt c = fromMaybe err $ lookup c $ zip (Map.keys ps) [0..]
  err = error "numberPaths: missing continue"
  in System
    { systemStart = mapContinues contToInt st
    , systemPaths = Map.mapKeys contToInt $ fmap (mapContinues contToInt) ps }

-- | Removes any paths which only serve to continue on to another path.
cleanContinues :: (Ord c) => System c b e a -> System c b e a
cleanContinues sys = let
  toClean = [ c | (c, Continue c') <- Map.toList $ systemPaths sys, c /= c' ]
  in foldr unrollSystem sys toClean
