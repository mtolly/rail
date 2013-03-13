-- | Generate Rail code from a control flow graph representation.
module Language.Rail.Generate
( stringLiteral
, literal
, result
, command
, function
, program
, toFile
) where

import Language.Rail.Base
import Data.ControlFlow
import Data.Char (isDigit, isPrint)
import qualified Data.Map as Map
import Text.Block
import Control.Monad.Trans.State
import Control.Monad (forM, forM_)
import Data.Array.ST
import Control.Monad.ST

-- | A string literal, to be read travelling east.
stringLiteral :: String -> String
stringLiteral s@[c] | isDigit c = s
stringLiteral s = "[" ++ concatMap f s ++ "]" where
  f c = case c of
    '\\' -> "\\\\"
    '\a' -> "\\a\\"
    '\b' -> "\\b\\"
    '\t' -> "\\t\\"
    '\n' -> "\\n\\"
    '\v' -> "\\v\\"
    '\f' -> "\\f\\"
    '\r' -> "\\r\\"
    '['  -> "\\[\\"
    ']'  -> "\\]\\"
    _    -> if isPrint c
      then [c]
      else let code = show $ fromEnum c in
        concat ["\\", code, reverse code, "\\"]

-- | A literal value, to be read travelling east.
literal :: Val -> String
literal (Str s) = stringLiteral s
literal (Int i) = stringLiteral $ show i
literal Nil = "n"
literal (Pair x y) = literal x ++ literal y ++ ":"

-- | A (successful or not) function ending, to be read travelling east.
result :: Result -> String
result Return       = "#"
result Boom         = "b"
result (Internal s) = stringLiteral s ++ "b"

-- | A non-end instruction, to be read travelling east.
command :: Command -> String
command c = case c of
  EOF -> "e"
  Input -> "i"
  Output -> "o"
  Underflow -> "u"
  Type -> "?"
  Push s -> "(" ++ s ++ ")"
  Pop s -> "(!" ++ s ++ "!)"
  Call s -> "{" ++ s ++ "}"
  Add -> "a"
  Div -> "d"
  Mult -> "m"
  Rem -> "r"
  Sub -> "s"
  Val v -> literal v
  Cut -> "c"
  Append -> "p"
  Size -> "z"
  Cons -> ":"
  Uncons -> "~"
  Greater -> "g"
  Equal -> "q"

-- | The minimum width needed to encode the given path.
pathWidth :: Path c () Result Command -> Int
pathWidth g = case g of
  End e         -> length $ result e
  Continue _    -> 0
  Branch () x y -> 5 + max (pathWidth x) (pathWidth y)
  x :>> xs      -> length (command x) + pathWidth xs

-- | The minimum width needed to encode all the paths inside the system.
systemWidth :: System c () Result Command -> Int
systemWidth (System st ps) = maximum $ map pathWidth $ st : Map.elems ps

-- | The number of leaf nodes in a path's tree, equal to the number of branches
-- plus 1.
leaves :: Path c () Result Command -> Int
leaves g = case g of
  Branch () x y -> leaves x + leaves y
  _ :>> xs      -> leaves xs
  _             -> 1

{-

Here is the kind of Rail code we will generate:

$  'function'
 \
  -[starting path]-\
                   |
 /-[next path]-----+-\
 |                 | |
 \-----------------/-+-\
                     | |
 /-\  /--------------+-/
 |  -<               |
 |    \-[end]#       +
 |                   |
 \-------------------/

This block will be generated in 4 subblocks:
* the left edge chunk: the first 3 columns
* the function name chunk: the first 2 rows except the first 3 columns
* the command chunk: in the code above, everything from [starting path] down
* the route chunk: everything to the right of the command chunk

The branch near the bottom takes up 5 columns, "\  /-" on the first line.
If the top branch splits into more branches, the branch can be extended like so:

 \  /-[top]-
  -<
    \
    |
    \-[bottom]-

-}

-- | Generates a branch, given the number of subpaths in the top branch.
branch :: Int -> Block
branch 1 = text $ unlines ["\\  /-", " -<", "   \\-"]
branch n = text $ unlines $
  ["\\  /-", " -<", "   \\"] ++ replicate ((n - 1) * 2 - 1) "   |" ++ ["   \\-"]

-- | Generates the code for a path or subpath, to be read travelling east.
pathBlock :: Int -> Path c () Result Command -> Block
pathBlock w p = case p of
  x :>> p' -> let b = line $ command x in horiz b $ pathBlock (w - width b) p'
  Branch () x y -> let
    b = branch $ leaves x
    w' = w - width b
    in horiz b $ vert (pathBlock w' x) $ vert (line "") $ pathBlock w' y
  End e -> line $ result e
  Continue _ -> line $ replicate w '-'

nameChunk :: String -> Block
nameChunk s = vert (line $ "'" ++ s ++ "'") (line "")

commandChunk :: Int -> System Int () Result Command -> Block
commandChunk w sys = let
  blank = line ""
  dashes = line $ replicate w '-'
  in foldr vert empty $
    [ pathBlock w $ systemStart sys
    , blank ]
    ++ concat [ [pathBlock w p, blank, dashes, blank]
              | p <- Map.elems $ systemPaths sys ]

leftChunk :: System Int () Result Command -> Block
leftChunk sys = let
  blank = line ""
  in foldr vert empty $
    [ line "$  "
    , line " \\ "
    , line "  -"
    , blank ]
    ++ concat [ [line " /-"] ++ replicate n (line " | ") ++ [line " \\-", blank]
              | p <- Map.elems $ systemPaths sys
              , let n = leaves p * 2 - 1 ]

----

routeChunk :: System Int () Result Command -> Block
routeChunk sys = let
  setRow :: Row -> State (Row, [Bridge]) ()
  setRow r = modify $ \(_, bs) -> (r, bs)
  addBridge :: Bridge -> State (Row, [Bridge]) ()
  addBridge b = modify $ \(r, bs) -> (r, b : bs)
  go :: Path Int () Result Command -> State (Row, [Bridge]) ()
  go path = case path of
    _ :>> path'   -> go path'
    Branch () x y -> go x >> go y
    Continue c    -> do
      (r, bs) <- get
      addBridge $ newBridge r (entranceRow c sys) bs
      setRow $ r + 2
    End _         -> modify $ \(r, bs) -> (r + 2, bs)
  goTop n path = setRow (exitRow n 0 sys) >> go path
  goStart path = setRow 0 >> go path
  bridges = snd $ execState f (0, [])
  f = do
    goStart $ systemStart sys
    forM_ (Map.toList $ systemPaths sys) $ uncurry goTop
  in drawBridges bridges

type Row    = Int -- ^ starts from 0 at the top of the route/command chunks
type Column = Int -- ^ starts from 0 at the left of the route chunk
type Bridge = (Row, Row, Column)

-- | Returns the row of a Continue leaf node, given its parent path ID, and a
-- number for which leaf node it is within the path (the topmost is 0).
exitRow :: Int -> Int -> System Int () Result Command -> Row
exitRow parent n sys = entranceRow (parent - 1) sys + 2 * (n + 1)

-- | Returns the row needed to enter the given path.
entranceRow :: Int -> System Int () Result Command -> Row
entranceRow n sys = let
  paths = take (n + 1) $ Map.elems $ systemPaths sys
  in sum [ 2 * leaves p + 2 | p <- paths ]

-- | Returns only the bridges which overlap the given source and destination.
overlapping :: Row -> Row -> [Bridge] -> [Bridge]
overlapping sr dr = filter $ \(sr', dr', _) ->
  any (\x -> compare sr' x /= compare dr' x) [sr, dr]

-- | Makes a bridge between the two rows at the lowest possible odd column.
newBridge :: Row -> Row -> [Bridge] -> Bridge
newBridge sr dr bs = let
  overCols = [ c | (_, _, c) <- overlapping sr dr bs ]
  in (sr, dr, head [ c | c <- [1, 3 ..], notElem c overCols ])

drawBridges :: [Bridge] -> Block
drawBridges [] = empty
drawBridges bs = runST $ do
  let maxRow = maximum [ r | (sr, dr, _) <- bs, r <- [sr, dr] ]
      maxCol = maximum [ c | (_, _, c) <- bs ]
  ary <- newArray ((0, 0), (maxRow, maxCol)) ' '
  forM_ bs $ \b -> drawBridge b ary
  arrayBlock ary

drawBridge :: Bridge -> STArray s (Int, Int) Char -> ST s ()
drawBridge (sr, dr, col) ary = do
  forM_ [0 .. col - 1] $ \c -> let
    ch = if odd c then '+' else '-'
    in writeArray ary (sr, c) ch >> writeArray ary (dr, c) ch
  if sr < dr
    then do
      writeArray ary (sr, col) '\\'
      writeArray ary (dr, col) '/'
      forM_ [sr + 1 .. dr - 1] $ \r -> let
        ch = if even r then '+' else '|'
        in writeArray ary (r, col) ch
    else do
      writeArray ary (dr, col) '\\'
      writeArray ary (sr, col) '/'
      forM_ [dr + 1 .. sr - 1] $ \r -> let
        ch = if even r then '+' else '|'
        in writeArray ary (r, col) ch

arrayBlock :: STArray s (Int, Int) Char -> ST s Block
arrayBlock ary = do
  ((r0, c0), (r1, c1)) <- getBounds ary
  let arrayLine r cf ct = forM [cf .. ct] $ \c -> readArray ary (r, c)
  ls <- forM [r0 .. r1] $ \r -> arrayLine r c0 c1
  return $ Block ls (c1 - c0 + 1) (r1 - r0 + 1)

----

functionBlock :: (Eq c) => String -> System c () Result Command -> Block
functionBlock name sys = let
  sys' = numberPaths sys
  in horiz (leftChunk sys') $
    vert (nameChunk name) $
      horiz (commandChunk (systemWidth sys) sys') $
        routeChunk sys'

function :: (Eq c) => String -> System c () Result Command -> String
function name sys = show $ functionBlock name sys

program :: (Eq c) => [(String, System c () Result Command)] -> String
program = removeEndSpace . concatMap (uncurry function)

removeEndSpace :: String -> String
removeEndSpace "" = ""
removeEndSpace str = case span (== ' ') str of
  (_  , "") -> ""
  (_  , '\n' : str') -> '\n' : removeEndSpace str'
  (sps, c    : str') -> sps ++ [c] ++ removeEndSpace str'

toFile :: (Eq c) => FilePath -> [(String, System c () Result Command)] -> IO ()
toFile fp = writeFile fp . program
