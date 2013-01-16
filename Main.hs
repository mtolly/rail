module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Language.Rail.Parse
import Language.Rail.Run
import Language.Rail.C

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [fin] -> readFile fin >>= runMemory . compile
  [fin, fout] -> do
    s <- readFile fin
    sout <- makeCFile $ getFunctions s
    writeFile fout sout
  _ -> mapM_ (hPutStrLn stderr)
    [ "usage: hrail file-in.rail            (directly interpret)"
    , "       hrail file-in.rail file-out.c (generate C code)" ]
