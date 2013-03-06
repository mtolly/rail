module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Language.Rail.Parse
import Language.Rail.Run
import qualified Language.Rail.C as C
import qualified Language.Rail.Generate as Gen

main :: IO ()
main = getArgs >>= \argv -> case argv of
  [fin] -> readFile fin >>= runMemory . compile
  ["-c", fin, fout] ->
    readFile fin >>= writeFile fout . C.makeFile . getFunctions
  ["-r", fin, fout] ->
    readFile fin >>= writeFile fout . Gen.program . getFunctions
  _ -> mapM_ (hPutStrLn stderr)
    [ "usage: hrail file-in.rail                  (directly interpret)"
    , "       hrail -c file-in.rail file-out.c    (generate C code)"
    , "       hrail -r file-in.rail file-out.rail (clean up Rail code)"
    ]
