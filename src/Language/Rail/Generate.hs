module Language.Rail.Generate where

import Language.Rail.Base
import Data.ControlFlow

-- | A string literal, to be read travelling east.
strLit :: String -> String
strLit s = "[" ++ concatMap f s ++ "]" where
  f c = case c of
    '\\' -> "\\\\"
    '\n' -> "\\n\\"
    '\t' -> "\\t\\"
    '['  -> "\\[\\"
    ']'  -> "\\]\\"
    _    -> [c]

varPush :: String -> String
varPush s = "(" ++ s ++ ")"

varPop :: String -> String
varPop s = "(!" ++ s ++ "!)"

funCall :: String -> String
funCall s = "{" ++ s ++ "}"
