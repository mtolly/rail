module Main where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Language.Rail.Base
import Language.Rail.Run
import Language.Rail.C
import Language.C.Syntax.AST
import Language.C.Pretty

main = getArgs >>= \argv -> case argv of
  [fin] -> readFile fin >>= runMemory . compile
  [fin, fout] -> do
    s <- readFile fin
    let tunit = CTranslUnit (makeProgram $ getFunctions s) unn
    head <- header
    let output = head ++ show (pretty $ tunit)
    writeFile fout output
  _ -> hPutStrLn stderr "usage: hrail <file>"
