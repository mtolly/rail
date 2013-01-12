module Language.Rail.Generate where

import Language.Rail.Base
import Data.ControlFlow
import Data.Char (isDigit)
import qualified Data.Map as Map
import Control.Monad (forM_)

import Control.Monad.ST
import Data.Array.ST

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

width :: Path c Result Command -> Int
width g = case g of
  End e      -> length $ end e
  Continue _ -> 0
  x :|| y    -> 5 + max (width x) (width y)
  x :>> xs   -> length (command x) + width xs

systemWidth :: System c Result Command -> Int
systemWidth (System st ps) = maximum $ map width $ st : Map.elems ps

paths :: Path c Result Command -> Int
paths g = case g of
  x :|| y  -> paths x + paths y
  _ :>> xs -> paths xs
  _        -> 1

{-

Here is the kind of Rail code we will generate:

$  .'function'   .
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

The dots on the first line mark the first and last columns of instructions.
The branch near the bottom takes up 5 columns, "\  /-" on the first line.

If the top branch splits into more branches, the branch can be extended like so:

 \  /-[top]-
  -<
    \
    |
    \-[bottom]-

-}

type Grid s = STUArray s Posn Char

-- | Takes height and width.
makeGrid :: Int -> Int -> ST s (Grid s)
makeGrid h w = newArray ((0, 0), (h - 1, w - 1)) ' '

writeStr :: Grid s -> Posn -> String -> ST s ()
writeStr _ _      ""     = return ()
writeStr g (r, c) (x:xs) = writeArray g (r, c) x >> writeStr g (r, c + 1) xs

-- | Generates a split path, given the top-left corner and the number of
-- branches in the top path. Returns the new beginnings of the top and bottom
-- branches.
writeBranch :: Grid s -> Posn -> Int -> ST s (Posn, Posn)
writeBranch g p@(r, c) h = do
  writeStr g p "\\  /-"
  writeStr g (r + 1, c) " -<"
  forM_ [1 .. h - 1] $ \n -> do
    writeArray g (r + 2 * n, c + 3) '\\'
    writeArray g (r + 2 * n + 1, c + 3) '|'
  writeStr g (r + 2 * h, c + 3) "\\-"
  let xp = (r, c + 5)
      yp = (r + 2 * h, c + 5)
  return (xp, yp)

-- | Takes a grid, top-left corner, width, and path.
writePath :: Grid s -> Posn -> Int -> Path c Result Command -> ST s ()
writePath g p@(r, c) w go = case go of
  x :>> xs -> let
    com = command x
    len = length com
    in writeStr g p com >> writePath g (r, c + len) (w - len) xs
  x :|| y -> do
    (xp, yp) <- writeBranch g p $ paths x
    writePath g xp (w - 5) x
    writePath g yp (w - 5) y
  End e -> writeStr g p (end e)
  Continue _ -> writeStr g p (replicate w '-')
