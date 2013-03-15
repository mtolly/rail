{-# LANGUAGE DeriveDataTypeable #-}
-- | Converts the textual grids of a Rail program to abstract control flow
-- graphs.
module Language.Rail.Parse
( Grid
, Posn
, Direction(..)
, makeGrid
, makeSystem
, functionName
, splitFunctions
, getFunctions
) where

import Language.Rail.Base
import Data.ControlFlow
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Array.Unboxed
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map
import Control.Monad.Trans.State
import Data.Data (Data, Typeable)

-- | A two-dimensional array, where the top-left corner is index @(0, 0)@.
type Grid = UArray Posn Char

-- | A grid position in @(row, column)@ format.
type Posn = (Int, Int)

-- | One of the eight orthogonal or diagonal directions.
data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

char :: Grid -> Posn -> Char
char g p = if inRange (bounds g) p then g ! p else ' '

-- | Converts a string to a two-dimensional grid, where newlines separate
-- the grid rows.
makeGrid :: String -> Grid
makeGrid str = let
  rows = lines $ fixTabs $ fixCR str
  fixTabs = intercalate "    " . splitOn "\t"
  fixCR = filter (/= '\r')
  height = length rows
  width = foldr (max . length) 0 rows
  pairs = concatMap (\(r, row) -> [((r, c), ch) | (c, ch) <- zip [0..] row])
    $ zip [0..] rows
  in listArray ((0, 0), (height - 1, width - 1)) (repeat ' ') // pairs

-- | Returns the contents of all squares in a straight line from a starting
-- position and direction, stopping at the bounds of the grid.
getString :: Grid -> Posn -> Direction -> [(Posn, Char)]
getString g p d = map (\pn -> (pn, g ! pn)) $
  takeWhile (inRange $ bounds g) $ iterate (`primary` d) p

-- | Takes a list of square contents starting from after a '[' or ']', and
-- reads a string literal which ends at the given end character.
readConstant :: [(Posn, Char)] -> Char -> Maybe (String, Posn)
readConstant pnchs endc = go "" pnchs where
  go str ((_, '\\') : pcs) = case span (\(_, ch) -> ch /= '\\') pcs of
    (esc, _ : pcs') -> case map snd esc of
      "" -> go ('\\' : str) pcs'
      "a" -> go ('\a' : str) pcs'
      "b" -> go ('\b' : str) pcs'
      "t" -> go ('\t' : str) pcs'
      "n" -> go ('\n' : str) pcs'
      "v" -> go ('\v' : str) pcs'
      "f" -> go ('\f' : str) pcs'
      "r" -> go ('\r' : str) pcs'
      "[" -> go ('[' : str) pcs'
      "]" -> go (']' : str) pcs'
      esc' -> let
        len = length esc'
        (l, r) = splitAt (quot len 2) esc'
        in if even len && l == reverse r && all isDigit l
          then go (toEnum (read l) : str) pcs'
          else Nothing -- unrecognized escape sequence
    _ -> Nothing -- no trailing backslash
  go str ((p, c) : pcs) = if c == endc
    then Just (reverse str, p)
    else go (c : str) pcs
  go _ [] = Nothing -- no end character

-- | True if the character is legal in a variable or function name.
varChar :: Char -> Bool
varChar = (`notElem` "{}!()'")

action :: Grid -> (Posn, Direction) -> Path (Posn, Direction) () Result Command
action g (p, d) = case char g p of
  '#' -> End Return
  'b' -> End Boom
  'e' -> EOF :>> movement
  'i' -> Input :>> movement
  'o' -> Output :>> movement
  'u' -> Underflow :>> movement
  '[' -> case getString g p d of
    _ : pcs -> case readConstant pcs ']' of
      Just (str, endp) -> Val (toVal str) :>> moveFrom endp
      Nothing -> lexerr "string literal"
    _ -> lexerr "string literal"
  ']' -> case getString g p d of
    _ : pcs -> case readConstant pcs '[' of
      Just (str, endp) -> Val (toVal str) :>> moveFrom endp
      Nothing -> lexerr "string literal"
    _ -> lexerr "string literal"
  ')' -> case getString g p d of
    _ : (_, '!') : str -> case span (varChar . snd) str of
      (fun, (_, '!') : (endp, '(') : _) ->
        Pop (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable set"
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '(') : _) ->
        Push (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable get"
    _ -> lexerr "variable get/set"
  '(' -> case getString g p d of
    _ : (_, '!') : str -> case span (varChar . snd) str of
      (fun, (_, '!') : (endp, ')') : _) ->
        Pop (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable set"
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, ')') : _) ->
        Push (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable get"
    _ -> lexerr "variable get/set"
  '{' -> case getString g p d of
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '}') : _) -> Call (map snd fun) :>> moveFrom endp
      _ -> lexerr "function call"
    _ -> lexerr "function call"
  '}' -> case getString g p d of
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '{') : _) -> Call (map snd fun) :>> moveFrom endp
      _ -> lexerr "function call"
    _ -> lexerr "function call"
  'a' -> Add :>> movement
  'd' -> Div :>> movement
  'm' -> Mult :>> movement
  'r' -> Rem :>> movement
  's' -> Sub :>> movement
  n | isDigit n -> Val (toVal [n]) :>> movement
  'c' -> Cut :>> movement
  'p' -> Append :>> movement
  'z' -> Size :>> movement
  'n' -> Val Nil :>> movement
  ':' -> Cons :>> movement
  '~' -> Uncons :>> movement
  '?' -> Type :>> movement
  'f' -> Val (toVal False) :>> movement
  'g' -> Greater :>> movement
  'q' -> Equal :>> movement
  't' -> Val (toVal True) :>> movement
  -- Y junctions
  'v' -> case d of
    N -> junction NW NE
    SE -> junction NE S
    SW -> junction S NW
    _ -> juncterr
  '^' -> case d of
    S -> junction SE SW
    NW -> junction SW N
    NE -> junction N SE
    _ -> juncterr
  '>' -> case d of
    W -> junction SW NW
    NE -> junction NW E
    SE -> junction E SW
    _ -> juncterr
  '<' -> case d of
    E -> junction NE SE
    SW -> junction SE W
    NW -> junction W NE
    _ -> juncterr
  _ -> movement
  where movement = moveFrom p
        moveFrom pn = either (End . Internal) Continue $ move g pn d
        junction dl dr = Branch () (force g p dl) (force g p dr)
        lexerr what = End $ Internal $ "lex error: invalid " ++ what
        juncterr = End $ Internal "internal junction error"

-- | Move out of a junction, as if via a primary connection. If the direction
-- can't be moved into, ends with an error.
force ::
  Grid -> Posn -> Direction -> Path (Posn, Direction) () Result Command
force g p d = let p' = primary p d in case tryPrimary d $ char g p' of
  Nothing -> End $ Internal "invalid movement out of junction"
  Just d' -> Continue (p', d')

-- | Moves out of a non-junction square, by either a primary or secondary
-- connection.
move :: Grid -> Posn -> Direction -> Either String (Posn, Direction)
move g p d = let
  pstraight = primary p d
  (pleft, pright) = secondary p d
  dstraight = tryPrimary d $ char g pstraight
  dleft = trySecondary d $ char g pleft
  dright = trySecondary d $ char g pright
  in case (dstraight, dleft, dright) of
    (Just s , _      , _      ) -> Right (pstraight, s)
    (Nothing, Just l , Nothing) -> Right (pleft, l)
    (Nothing, Nothing, Just r ) -> Right (pright, r)
    (Nothing, Nothing, Nothing) -> Left "no possible movement"
    _                           -> Left "ambiguous movement"

-- | The square located in the primary direction.
primary :: Posn -> Direction -> Posn
primary (r, c) d = (r + dr, c + dc) where
  dr = case d of SW -> 1; S -> 1; SE -> 1; W -> 0; E -> 0; _ -> -1
  dc = case d of NE -> 1; E -> 1; SE -> 1; N -> 0; S -> 0; _ -> -1

-- | The squares located in the two secondary directions; respectively, angled
-- left and angled right from the primary direction.
secondary :: Posn -> Direction -> (Posn, Posn)
secondary p d = (primary p $ clockwise (-1) d, primary p $ clockwise 1 d)

-- | Turns a direction n ticks in the clockwise direction. Use negative numbers
-- for counter-clockwise.
clockwise :: Int -> Direction -> Direction
clockwise n = toEnum . (`mod` 8) . (+ n) . fromEnum

-- | If the char can be entered directly from the given direction, returns the
-- train's new direction.
tryPrimary :: Direction -> Char -> Maybe Direction
tryPrimary d c = lookup d $ case c of
  '-'  -> zip [E, NE, SE, W, NW, SW] [E,  E,  E,  W,  W,  W]
  '|'  -> zip [N, NE, NW, S, SE, SW] [N,  N,  N,  S,  S,  S]
  '/'  -> zip [N, E,  NE, S, W,  SW] [NE, NE, NE, SW, SW, SW]
  '\\' -> zip [N, W,  NW, S, E,  SE] [NW, NW, NW, SE, SE, SE]
  '@'  -> let dirs = [minBound .. maxBound] in zip dirs $ map (clockwise 4) dirs
  '+'  -> let dirs = [N,  S,  E,  W]  in zip dirs dirs
  'x'  -> let dirs = [NW, NE, SW, SE] in zip dirs dirs
  'v'  -> let dirs = [N, SE, SW] in zip dirs dirs
  '^'  -> let dirs = [S, NW, NE] in zip dirs dirs
  '>'  -> let dirs = [W, SE, NE] in zip dirs dirs
  '<'  -> let dirs = [E, NW, SW] in zip dirs dirs
  _    -> if elem c "*beiou?[](){}admrs0123456789cpzn:~fgqt#"
    then [(d, d)] -- universal junction
    else []       -- rubble

-- | If the char can be entered indirectly by a train which is facing the given
-- direction, returns the train's new direction.
trySecondary :: Direction -> Char -> Maybe Direction
trySecondary d c = lookup d $ case c of
  '-'  -> zip [NE, SE, NW, SW] [E,  E,  W,  W]
  '|'  -> zip [NE, NW, SE, SW] [N,  N,  S,  S]
  '/'  -> zip [N,  E,  S,  W]  [NE, NE, SW, SW]
  '\\' -> zip [N,  W,  S,  E]  [NW, NW, SE, SE]
  _ -> [] -- can't be a secondary connection

-- | Reads a single function from a grid. The function beginning (@$@) must be
-- at @(0, 0)@, going 'SE'.
makeSystem :: Grid -> System (Posn, Direction) () Result Command
makeSystem g = let
  dlr = ((0, 0), SE)
  -- We recursively add only the paths which are actually used in the function.
  -- This means we don't have to call cleanPaths.
  startFrom pd = gets (Map.lookup pd) >>= \mg -> case mg of
    Just _ -> return ()
    Nothing -> do
      let act = action g pd
      modify $ Map.insert pd act
      mapM_ startFrom $ continues act
  in cleanContinues $ simplifyPaths $
    System (Continue dlr) $ execState (startFrom dlr) Map.empty

-- | Extracts the function name from the text of a single function.
functionName :: String -> Maybe String
functionName ('\'' : cs) = case span varChar cs of
  (fun, '\'' : _) -> Just fun
  _               -> Nothing
functionName (c : cs) | c /= '\n' = functionName cs
functionName _ = Nothing

-- | Splits the text of a Rail file into function.
splitFunctions :: String -> [String]
splitFunctions prog = case splitOn "\n$" $ '\n' : prog of
  x@('$':_) : xs -> x : map ('$' :) xs
  _ : xs -> map ('$' :) xs
  [] -> []

-- | Parses all the functions defined in the text of a Rail file.
getFunctions :: String -> [(String, System (Posn, Direction) () Result Command)]
getFunctions = mapMaybe f . splitFunctions
  where f str = fmap (\n -> (n, makeSystem $ makeGrid str)) $ functionName str
