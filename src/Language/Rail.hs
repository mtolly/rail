module Language.Rail where

import Data.ControlFlow
import qualified Data.Map as Map
import Data.Array.Unboxed
import Data.Char (isDigit, isSpace)
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import System.IO (isEOF)
import System.IO.Error (catchIOError, isEOFError)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn)
import Data.List (intercalate)

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

data Command
  = Boom
  | EOF
  | Input
  | Output
  | Underflow
  | Type
  | Push String -- ^ read a var, push to stack
  | Pop String -- ^ pop from stack, store in var
  | Call String -- ^ function call
  | Add
  | Div
  | Mult
  | Rem
  | Sub
  | Val Val -- ^ push a constant value
  | Cut
  | Append
  | Size
  | Cons
  | Uncons
  | Greater
  | Equal
  | SetBranch -- ^ pop a bool value, set the state's condition
  deriving (Eq, Ord, Show, Read)
  -- TODO: lambda

type Sub = Flow (Maybe String) Command

-- | TODO: lambda
data Val
  = Str String
  | Nil
  | Pair Val Val
  deriving (Eq, Ord, Show, Read)

type Posn = (Int, Int)
data Direction = N | NE | E | SE | S | SW | W | NW
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
type Grid = UArray Posn Char

char :: Grid -> Posn -> Char
char g p = if inRange (bounds g) p then g ! p else ' '

makeGrid :: String -> Grid
makeGrid str = let
  rows = lines $ fixTabs $ fixCR str
  fixTabs = intercalate "    " . splitOn "\t"
  fixCR = filter (/= '\r')
  height = length rows
  width = foldr (max . length) 0 rows
  pairs = concatMap (\(r, row) -> [((r, c), ch) | (c, ch) <- zip [0..] row])
    $ zip [0..] rows
  in listArray ((0, 0), (height - 1, width - 1)) (repeat ' ') // pairs

-- | Returns the contents of all squares in a straight line from a starting
-- position and direction, stopping at the bounds of the grid.
getString :: Grid -> Posn -> Direction -> [(Posn, Char)]
getString g p d = map (\pn -> (pn, g ! pn)) $
  takeWhile (inRange $ bounds g) $ iterate (`primary` d) p

-- | Takes a list of square contents starting from after a '[' or ']', and
-- reads a string literal which ends at the given end character.
readConstant :: [(Posn, Char)] -> Char -> Maybe (String, Posn)
readConstant pnchs endc = go "" pnchs where
  go str ((_, '\\') : (_, '\\') : pcs) =
    go ('\\' : str) pcs
  go str ((_, '\\') : (_, c) : (_, '\\') : pcs) = case c of
    'n' -> go ('\n' : str) pcs
    't' -> go ('\t' : str) pcs
    _ -> go (c : str) pcs
  go str ((p, c) : pcs) = if c == endc
    then Just (reverse str, p)
    else go (c : str) pcs
  go _ [] = Nothing

-- | True if the character is legal in a variable or function name.
varChar :: Char -> Bool
varChar = (`notElem` "{}!()'")

action ::
  Grid -> (Posn, Direction) -> Go (Posn, Direction) (Maybe String) Command
action g (p, d) = case char g p of
  '#' -> End Nothing
  'b' -> Boom :>> movement
  'e' -> EOF :>> movement
  'i' -> Input :>> movement
  'o' -> Output :>> movement
  'u' -> Underflow :>> movement
  '[' -> case getString g p d of
    _ : pcs -> case readConstant pcs ']' of
      Just (str, endp) -> Val (Str str) :>> moveFrom endp
      Nothing -> lexerr "string literal"
    _ -> lexerr "string literal"
  ']' -> case getString g p d of
    _ : pcs -> case readConstant pcs '[' of
      Just (str, endp) -> Val (Str str) :>> moveFrom endp
      Nothing -> lexerr "string literal"
    _ -> lexerr "string literal"
  ')' -> case getString g p d of
    _ : (_, '!') : str -> case span (varChar . snd) str of
      (fun, (_, '!') : (endp, '(') : _) ->
        Pop (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable set"
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '(') : _) -> 
        Push (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable get"
    _ -> lexerr "variable get/set"
  '(' -> case getString g p d of
    _ : (_, '!') : str -> case span (varChar . snd) str of
      (fun, (_, '!') : (endp, ')') : _) ->
        Pop (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable set"
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, ')') : _) -> 
        Push (map snd fun) :>> moveFrom endp
      _ -> lexerr "variable get"
    _ -> lexerr "variable get/set"
  '{' -> case getString g p d of
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '}') : _) -> Call (map snd fun) :>> moveFrom endp
      _ -> lexerr "function call"
    _ -> lexerr "function call"
  '}' -> case getString g p d of
    _ : str -> case span (varChar . snd) str of
      (fun, (endp, '{') : _) -> Call (map snd fun) :>> moveFrom endp
      _ -> lexerr "function call"
    _ -> lexerr "function call"
  'a' -> Add :>> movement
  'd' -> Div :>> movement
  'm' -> Mult :>> movement
  'r' -> Rem :>> movement
  's' -> Sub :>> movement
  n | isDigit n -> Val (Str [n]) :>> movement
  'c' -> Cut :>> movement
  'p' -> Append :>> movement
  'z' -> Size :>> movement
  'n' -> Val Nil :>> movement
  ':' -> Cons :>> movement
  '~' -> Uncons :>> movement
  '?' -> Type :>> movement
  'f' -> Val (Str "0") :>> movement
  'g' -> Greater :>> movement
  'q' -> Equal :>> movement
  't' -> Val (Str "1") :>> movement
  -- Y junctions
  'v' -> SetBranch :>> case d of
    N -> junction NW NE
    SE -> junction NE S
    SW -> junction S NW
    _ -> juncterr
  '^' -> SetBranch :>> case d of
    S -> junction SE SW
    NW -> junction SW N
    NE -> junction N SE
    _ -> juncterr
  '>' -> SetBranch :>> case d of
    W -> junction SW NW
    NE -> junction NW E
    SE -> junction E SW
    _ -> juncterr
  '<' -> SetBranch :>> case d of
    E -> junction NE SE
    SW -> junction SE W
    NW -> junction W NE
    _ -> juncterr
  -- TODO: lambda
  _ -> movement
  where movement = moveFrom p
        moveFrom pn = either (End . Just) Continue $ move g pn d
        junction dl dr = force g p dl :|| force g p dr
        lexerr what = End $ Just $ "lex error: invalid " ++ what
        juncterr = End $ Just "internal junction error"

-- | Move out of a junction, as if via a primary connection. If the direction
-- can't be moved into, ends with an error.
force ::
  Grid -> Posn -> Direction -> Go (Posn, Direction) (Maybe String) Command
force g p d = let p' = primary p d in case tryPrimary d $ char g p' of
  Nothing -> End $ Just "invalid movement out of junction"
  Just d' -> Continue (p', d')

-- | Moves out of a non-junction square, by either a primary or secondary
-- connection.
move :: Grid -> Posn -> Direction -> Either String (Posn, Direction)
move g p d = let
  pstraight = primary p d
  (pleft, pright) = secondary p d
  dstraight = tryPrimary d $ char g pstraight
  dleft = trySecondary d $ char g pleft
  dright = trySecondary d $ char g pright
  in case (dstraight, dleft, dright) of
    (Just s , _      , _      ) -> Right (pstraight, s)
    (Nothing, Just l , Nothing) -> Right (pleft, l)
    (Nothing, Nothing, Just r ) -> Right (pright, r)
    (Nothing, Nothing, Nothing) -> Left "no possible movement"
    _                           -> Left "ambiguous movement"

-- | The square located in the primary direction.
primary :: Posn -> Direction -> Posn
primary (r, c) d = (r + dr, c + dc) where
  dr = case d of SW -> 1; S -> 1; SE -> 1; W -> 0; E -> 0; _ -> -1
  dc = case d of NE -> 1; E -> 1; SE -> 1; N -> 0; S -> 0; _ -> -1

-- | The squares located in the two secondary directions; respectively, angled
-- left and angled right from the primary direction.
secondary :: Posn -> Direction -> (Posn, Posn)
secondary p d = (primary p $ clockwise (-1) d, primary p $ clockwise 1 d)

-- | Turns a direction n ticks in the clockwise direction. Use negative numbers
-- for counter-clockwise.
clockwise :: Int -> Direction -> Direction
clockwise n = toEnum . (`mod` 8) . (+ n) . fromEnum

-- | If the char can be entered directly from the given direction, returns the
-- train's new direction.
tryPrimary :: Direction -> Char -> Maybe Direction
tryPrimary d c = lookup d $ case c of
  ' '  -> [] -- space is the only char which can't be a primary connection
  '-'  -> zip [E, NE, SE, W, NW, SW] [E, E, E, W, W, W]
  '|'  -> zip [N, NE, NW, S, SE, SW] [N, N, N, S, S, S]
  '/'  -> zip [N, E,  NE, S, W,  SW] [NE, NE, NE, SW, SW, SW]
  '\\' -> zip [N, W,  NW, S, E,  SE] [NW, NW, NW, SE, SE, SE]
  '+'  -> let dirs = [N,  S,  E,  W]  in zip dirs dirs
  'x'  -> let dirs = [NW, NE, SW, SE] in zip dirs dirs
  '@'  -> let dirs = [minBound..maxBound] in zip dirs $ map (clockwise 4) dirs
  'v'  -> let dirs = [N, SE, SW] in zip dirs dirs
  '^'  -> let dirs = [S, NW, NE] in zip dirs dirs
  '>'  -> let dirs = [W, SE, NE] in zip dirs dirs
  '<'  -> let dirs = [E, NW, SW] in zip dirs dirs
  _ -> [(d, d)] -- anything else is a universal junction

-- | If the char can be entered indirectly by a train which is facing the given
-- direction, returns the train's new direction.
trySecondary :: Direction -> Char -> Maybe Direction
trySecondary d c = lookup d $ case c of
  '-'  -> zip [NE, SE, NW, SW] [E,  E,  W,  W]
  '|'  -> zip [NE, NW, SE, SW] [N,  N,  S,  S]
  '/'  -> zip [N,  E,  S,  W]  [NE, NE, SW, SW]
  '\\' -> zip [N,  W,  S,  E]  [NW, NW, SE, SE]
  _ -> [] -- can't be a secondary connection

makeSystem :: Grid -> System (Posn, Direction) (Maybe String) Command
makeSystem g = let
  pds = [ (p, d) | p <- indices g, d <- [minBound .. maxBound] ]
  paths = Map.fromList $ zip pds $ map (action g) pds
  in System (Continue ((0, 0), SE)) paths

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

