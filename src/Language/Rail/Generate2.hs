{-# LANGUAGE RankNTypes #-}
module Language.Rail.Generate where

import Data.ControlFlow
import Language.Rail.Base
import Language.Rail.Parse
  ( Posn
  , Direction(..)
  , primary
  )

import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Char (isDigit)

type Grid s = STArray s Posn Char

data Bug s = Bug
  { grid :: Grid s
  , posn :: Posn
  , direction :: Direction
  }

type Gen s = StateT (Bug s) (ST s)

setPosn :: Posn -> Gen s ()
setPosn p = modify $ \b -> b { posn = p }

face :: Direction -> Gen s ()
face d = modify $ \b -> b { direction = d }

-- | Write a string starting from the current location, and update the position
-- to be the square after the string.
write :: String -> Gen s ()
write = mapM_ writeChar where
  writeChar c = do
    b <- get
    lift $ writeArray (grid b) (posn b) c
    put $ b { posn = primary (posn b) (direction b) }

-- | Runs a grid bug action on a blank grid of the given height and width, and
-- returns the final array.
runGrid :: Int -> Int -> (forall s. Gen s ()) -> Array Posn Char
runGrid h w gen = runSTArray $ do
  g <- newArray ((0, 0), (h - 1, w - 1)) ' '
  evalStateT gen $ Bug g (0, 0) SE
  return g

----------

-- | A string literal.
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

-- | A literal value, which pushes some value onto the stack.
literal :: Val -> String
literal (Str s) = strLit s
literal Nil = "n"
literal (Pair x y) = literal x ++ literal y ++ ":"

-- | A (successful or not) function ending.
end :: Result -> String
end Return       = "#"
end Boom         = "b"
end (Internal s) = strLit s ++ "b"

-- | A non-end instruction.
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
