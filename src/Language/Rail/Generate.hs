module Language.Rail.Generate where

import Language.Rail.Base hiding (Grid)
import Data.ControlFlow
import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray

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

width :: Go c Result Command -> Int
width g = case g of
  End e      -> length $ end e
  Continue _ -> 0
  x :|| y    -> 4 + max (width x) (width y)
  x :>> xs   -> length (command x) + width xs

systemWidth :: System c Result Command -> Int
systemWidth (System st ps) = maximum $ map width $ st : Map.elems ps

paths :: Go c Result Command -> Int
paths g = case g of
  x :|| y  -> paths x + paths y
  _ :>> xs -> paths xs
  _        -> 1

type Grid s = STUArray s Posn Char
