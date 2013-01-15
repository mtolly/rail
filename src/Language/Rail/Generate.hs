module Language.Rail.Generate where

import Language.Rail.Base
import Data.ControlFlow
import Data.Char (isDigit)
import qualified Data.Map as Map
import Text.Block

-- | A string literal, to be read travelling east.
strLit :: String -> String
strLit s@[c] | isDigit c = s
strLit s = "[" ++ concatMap f s ++ "]" where
  f c = case c of
    '\\' -> "\\\\"
    '\n' -> "\\n\\"
    '\t' -> "\\t\\"
    '['  -> "\\[\\"
    ']'  -> "\\]\\"
    _    -> [c]

-- | A literal value, to be read travelling east.
literal :: Val -> String
literal (Str s) = strLit s
literal Nil = "n"
literal (Pair x y) = literal x ++ literal y ++ ":"

-- | A (successful or not) function ending, to be read travelling east.
end :: Result -> String
end Return       = "#"
end Boom         = "b"
end (Internal s) = strLit s ++ "b"

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
  End e         -> length $ end e
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
  End e -> line $ end e
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
    ++ concat [ [line " /-"] ++ replicate pipes (line " | ") ++ [line " \\-"]
              | p <- Map.elems $ systemPaths sys
              , let pipes = leaves p * 2 - 1 ]

routeChunk :: System Int () Result Command -> Block
routeChunk _ = empty

functionBlock :: (Eq c) => String -> System c () Result Command -> Block
functionBlock name sys = let
  sys' = numberPaths sys
  in horiz (leftChunk sys') $
    vert (nameChunk name) $
      horiz (commandChunk (systemWidth sys) sys') $
        routeChunk sys'
