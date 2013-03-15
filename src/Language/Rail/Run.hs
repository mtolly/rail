-- | A direct interpreter for Rail programs.
module Language.Rail.Run
( Memory(..)
, emptyMemory
, Rail
, runRail
, runCommand
, run
, call
, compile
, runMemory
) where

import Data.ControlFlow
import Language.Rail.Base
import Language.Rail.Parse (getFunctions)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import System.IO (isEOF, hPutStr, stderr, hFlush, stdout)
import System.IO.Error (isEOFError)
import qualified Control.Exception (catch)
import Data.Void (absurd)

-- | The memory state of a running Rail program.
data Memory m = Memory
  { stack     :: [Val]
  , variables :: Map.Map String Val
  , functions :: Map.Map String (Flow () Result Command)
  , executor  :: Command -> Rail m ()
  -- ^ The function which executes each 'Command' in the chosen monad.
  }

-- | A memory state with no defined variables or functions, and an empty stack.
emptyMemory :: Memory IO
emptyMemory = Memory
  { stack     = []
  , variables = Map.empty
  , functions = Map.empty
  , executor  = runCommand
  }

-- | A monad transformer for executing Rail programs, combining 'ErrorT'
-- (for crash messages) with 'StateT' (for the stack and variables).
type Rail m = StateT (Memory m) (ErrorT String m)

-- | Unwraps the Rail monad, executing its side-effects in the IO monad.
runRail :: Rail IO () -> Memory IO -> IO ()
runRail r mem = runErrorT (evalStateT r mem) >>= either (hPutStr stderr) return

err :: (Monad m) => String -> Rail m a
err = lift . throwError

setStack :: (Monad m) => [Val] -> Rail m ()
setStack stk = modify $ \mem -> mem { stack = stk }

setVariables :: (Monad m) => Map.Map String Val -> Rail m ()
setVariables vs = modify $ \mem -> mem { variables = vs }

getVar :: (Monad m) => String -> Rail m Val
getVar v = gets variables >>= \vs -> case Map.lookup v vs of
  Just x -> return x
  Nothing -> err $ "getVar: undefined variable: " ++ v

setVar :: (Monad m) => String -> Val -> Rail m ()
setVar v x = modify $ \mem ->
  mem { variables = Map.insert v x $ variables mem }

push :: (Monad m) => Val -> Rail m ()
push x = gets stack >>= setStack . (x :)

pop :: (Monad m) => Rail m Val
pop = gets stack >>= \stk -> case stk of
  [] -> err "pop: empty stack"
  x : xs -> setStack xs >> return x

-- | Pops a value and tries to convert it to a Haskell type. Otherwise, throws
-- an error message which includes the given type name.
popAs :: (Monad m, Value a) => String -> Rail m a
popAs typ = pop >>= maybe (err $ "popAs: expected " ++ typ) return . fromVal

popStr :: (Monad m) => Rail m String
popStr = popAs "string"

popInt :: (Monad m) => Rail m Integer
popInt = popAs "integer"

popBool :: (Monad m) => Rail m Bool
popBool = popAs "bool"

popPair :: (Monad m) => Rail m (Val, Val)
popPair = popAs "pair"

-- | Executes a single Rail instruction, in the IO monad.
runCommand :: Command -> Rail IO ()
runCommand c = case c of
  Val x -> push x
  Call fun -> gets functions >>= \funs -> case Map.lookup fun funs of
    Nothing -> err $ "call: undefined function " ++ fun
    Just sub -> call sub
  Add -> math (+)
  Sub -> math (-)
  Mult -> math (*)
  Div -> math div
  Rem -> math mod
  EOF -> liftIO isEOF >>= push . toVal
  Output -> popStr >>= liftIO . putStr >> liftIO (hFlush stdout)
  Input -> liftIO getChar' >>= \mc -> case mc of
    Just ch -> push $ toVal [ch]
    Nothing -> err "input: end of file"
  Equal -> liftM2 equal pop pop >>= push . toVal
  Greater -> liftM2 (<) popInt popInt >>= push . toVal
  -- (<) because flipped args: we're asking (2nd top of stack) > (top of stack)
  Underflow -> gets stack >>= push . toVal . length
  Type -> pop >>= \v -> push $ toVal $ case v of
    Str _ _ -> "string"
    Nil -> "nil"
    Pair _ _ -> "list"
  Cons -> liftM2 (flip Pair) pop pop >>= push
  Uncons -> popPair >>= \(x, y) -> push x >> push y
  Size -> popStr >>= push . toVal . length
  Append -> liftM2 (flip (++)) popStr popStr >>= push . toVal
  Cut -> do
    i <- fmap fromIntegral popInt
    s <- popStr
    if 0 <= i && i <= length s
      then case splitAt i s of
        (x, y) -> push (toVal x) >> push (toVal y)
      else err "cut: string index out of bounds"
  Push var -> getVar var >>= push
  Pop var -> pop >>= setVar var

-- | Like 'getChar', but catches EOF exceptions and returns Nothing.
getChar' :: IO (Maybe Char)
getChar' = Control.Exception.catch (fmap Just getChar) $ \e ->
  if isEOFError e then return Nothing else ioError e

math :: (Monad m) => (Integer -> Integer -> Integer) -> Rail m ()
math op = liftM2 (flip op) popInt popInt >>= push . toVal

equal :: Val -> Val -> Bool
equal (Str _ (Just x)) (Str _ (Just y)) = x == y
equal x y = x == y

-- | Runs a piece of code. Does not create a new scope.
run :: (Monad m) => Flow () Result Command -> Rail m ()
run g = case g of
  x :>> c       -> gets executor >>= ($ x) >> run c
  Branch () x y -> popBool >>= \b -> run $ if b then y else x
  Continue c    -> absurd c
  End e         -> case e of
    Return -> return ()
    Boom -> popStr >>= err
    Internal s -> err s

-- | Runs a function, which creates a new scope for the length of the function.
call :: (Monad m) => Flow () Result Command -> Rail m ()
call sub = gets variables >>= \vs ->
  setVariables Map.empty >> run sub >> setVariables vs

-- | Given a Rail file, add its functions to an empty memory state.
compile :: String -> Memory IO
compile str = emptyMemory
  { functions = Map.fromList $ map (mapSnd flow) $ getFunctions str }
  where mapSnd f (x, y) = (x, f y)

-- | Runs a memory state that has a \"main\" function defined.
runMemory :: Memory IO -> IO ()
runMemory = runRail $ run $ Call "main" :>> End Return
