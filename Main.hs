module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Language.Rail

main = getArgs >>= \argv -> case argv of
  f : _ -> readFile f >>= runMemory . compile
  _ -> hPutStrLn stderr "usage: hrail <file>"
