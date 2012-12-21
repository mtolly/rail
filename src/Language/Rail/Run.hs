module Language.Rail.Run where

import Data.ControlFlow
import Language.Rail.Base
import qualified Data.Map as Map
import Data.Char (isSpace)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import System.IO (isEOF)
import System.IO.Error (catchIOError, isEOFError)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)

data Memory = Memory
  { stack     :: [Val]
  , variables :: Map.Map String Val
  , functions :: Map.Map String Sub
  , condition :: Bool -- ^ used for branches
  }

emptyMemory :: Memory
emptyMemory = Memory
  { stack     = []
  , variables = Map.empty
  , condition = False
  , functions = Map.empty
  }

type Sub = Flow (Maybe String) Command

-- | Compiles a single subroutine. The starting point ($) must be at (0, 0).
makeSub :: Grid -> Sub
makeSub = flow . makeSystem

-- | A monad for executing Rail programs, combining ErrorT (for crash messages)
-- with StateT (for the stack and variables).
type Rail = StateT Memory (ErrorT String IO)

runRail :: Rail () -> Memory -> IO ()
runRail r mem = runErrorT (evalStateT r mem) >>= either putStrLn return

err :: String -> Rail a
err = lift . throwError

setStack :: [Val] -> Rail ()
setStack stk = modify $ \mem -> mem { stack = stk }

setVariables :: Map.Map String Val -> Rail ()
setVariables vs = modify $ \mem -> mem { variables = vs }

getVar :: String -> Rail Val
getVar v = gets variables >>= \vs -> case Map.lookup v vs of
  Just x -> return x
  Nothing -> err $ "getVar: undefined variable: " ++ v

setVar :: String -> Val -> Rail ()
setVar v x = modify $ \mem ->
  mem { variables = Map.insert v x $ variables mem }

push :: Val -> Rail ()
push x = gets stack >>= setStack . (x :)

pop :: Rail Val
pop = gets stack >>= \stk -> case stk of
  [] -> err "pop: empty stack"
  x : xs -> setStack xs >> return x

popStr :: Rail String
popStr = pop >>= \v -> case v of
  Str s -> return s
  _ -> err "popStr: expected string"

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(n, sp)] | all isSpace sp -> Just n
  _ -> Nothing

popInt :: Rail Integer
popInt = popStr >>= \s -> case readMaybe s of
  Just i -> return i
  Nothing -> err "popInt: couldn't read string as integer"

popBool :: Rail Bool
popBool = popStr >>= \s -> case s of
  "0" -> return False
  "1" -> return True
  _ -> err "popBool: expected \"0\" or \"1\""

popPair :: Rail (Val, Val)
popPair = pop >>= \v -> case v of
  Pair x y -> return (x, y)
  _ -> err "popPair: expected pair"

pushInt :: Integer -> Rail ()
pushInt = push . Str . show

pushBool :: Bool -> Rail ()
pushBool b = push $ Str $ if b then "1" else "0"

runCommand :: Command -> Rail ()
runCommand c = case c of
  Val x -> push x
  SetBranch -> popBool >>= \b -> modify $ \mem -> mem { condition = b }
  Call fun -> gets functions >>= \funs -> case Map.lookup fun funs of
    Nothing -> err $ "call: undefined function " ++ fun
    Just sub -> call sub
  Add -> math (+)
  Sub -> math (-)
  Mult -> math (*)
  Div -> math div
  Rem -> math mod
  Boom -> popStr >>= err
  EOF -> liftIO isEOF >>= \b -> push $ Str $ if b then "1" else "0"
  Output -> popStr >>= liftIO . putStr
  Input -> liftIO getChar' >>= \mc -> case mc of
    Just ch -> push $ Str [ch]
    Nothing -> err "input: end of file"
  Equal -> liftA2 (==) pop pop >>= pushBool
  Greater -> liftA2 (<) popInt popInt >>= pushBool -- (<) because flipped args
  Underflow -> gets stack >>= pushInt . fromIntegral . length
  Type -> pop >>= \v -> push $ Str $ case v of
    Str _ -> "string"
    Nil -> "nil"
    Pair _ _ -> "list"
    -- TODO: lambda
  Cons -> liftA2 (flip Pair) pop pop >>= push
  Uncons -> popPair >>= \(x, y) -> push y >> push x
  Size -> popStr >>= pushInt . fromIntegral . length
  Append -> liftA2 (flip (++)) popStr popStr >>= push . Str
  Cut -> do
    i <- fmap fromIntegral popInt
    s <- popStr
    if 0 <= i && i <= length s
      then case splitAt i s of
        (x, y) -> push (Str y) >> push (Str x)
      else err "cut: string index out of bounds"
  Push var -> getVar var >>= push
  Pop var -> pop >>= setVar var

-- | Like getChar, but catches EOF exceptions and returns Nothing.
getChar' :: IO (Maybe Char)
getChar' = catchIOError (fmap Just getChar) $ \e ->
  if isEOFError e then return Nothing else ioError e

math :: (Integer -> Integer -> Integer) -> Rail ()
math op = liftA2 (flip op) popInt popInt >>= push . Str . show

-- | Runs a piece of code. Does not create a new scope.
run :: Sub -> Rail ()
run g = case g of
  x :>> c -> runCommand x >> run c
  x :|| y -> gets condition >>= \b -> run $ if b then y else x
  Continue _ -> return ()
  End e -> maybe (return ()) (lift . throwError) e

-- | Runs a function, which creates a new scope for the length of the function.
call :: Sub -> Rail ()
call sub = gets variables >>= \vs ->
  setVariables Map.empty >> run sub >> setVariables vs

-- | Extracts the function name from the text of a single function.
functionName :: String -> Maybe String
functionName ('\'' : cs) = case span varChar cs of
  (fun, '\'' : _) -> Just fun
  _               -> Nothing
functionName (c : cs) | c /= '\n' = functionName cs
functionName _ = Nothing

splitFunctions :: String -> [String]
splitFunctions prog = case splitOn "\n$" ('\n' : prog) of
  x : xs -> x : map ('$' :) xs
  [] -> []

getFunctions :: String -> [(String, Sub)]
getFunctions = mapMaybe f . splitFunctions
  where f str = withSnd (makeSub $ makeGrid str) <$> functionName str
        withSnd y x = (x, y)

compile :: String -> Memory
compile str = emptyMemory { functions = Map.fromList $ getFunctions str }

runMemory :: Memory -> IO ()
runMemory = runRail $ run $ Call "main" :>> End Nothing
