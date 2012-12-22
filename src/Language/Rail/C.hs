-- | Rail to C compiler.
module Language.Rail.C where

import Data.ControlFlow
import Language.Rail.Base
import Paths_rail (getDataFileName)
import Data.Char (toLower, isAscii, isAlphaNum)
import Language.C.Syntax
import Language.C.Data.Node
import Language.C.Data.Ident

-- | Read the C Rail runtime.
header :: IO String
header = getDataFileName "header.c" >>= readFile

-- Convenience functions for Language.C.Syntax.AST

idt :: String -> Ident
idt = internalIdent

unn :: NodeInfo
unn = undefNode

var :: String -> CExpr
var str = CVar (idt str) unn

-- Name manglers

-- | Translates each string to a unique one consisting only of ASCII letters,
-- numbers, and underscores. It may start with an underscore, so for C
-- portability, add a prefix that starts with a non-underscore.
mangle :: String -> String
mangle = concatMap $ \c ->
  if isAscii c && isAlphaNum c
    then [c]
    else if c == '_'
      then "__"
      else "_" ++ show (fromEnum c) ++ "_"

varName :: String -> String
varName v = "var_" ++ mangle v

funName :: String -> String
funName f = "fun_" ++ mangle f

builtinName :: String -> String
builtinName b = "builtin_" ++ mangle b

-- | Turns ((4, 7), NE) into "NE_4_7".
makeLabel :: (Posn, Direction) -> String
makeLabel ((r, c), d) = show d ++ "_" ++ show r ++ "_" ++ show c

-- Rail to C compiler

-- | For variable foo, make the declaration "struct value *var_foo = NULL;"
vardecl :: String -> CDecl
vardecl v = let
  structType =
    CSUType (CStruct CStructTag (Just $ idt "value") Nothing [] unn) unn
  cdecl = CDeclr (Just $ idt $ varName v) [CPtrDeclr [] unn] Nothing [] unn
  cinit = CInitExpr (CConst $ CIntConst (cInteger 0) unn) unn
  in CDecl [CTypeSpec structType] [(Just cdecl, Just cinit, Nothing)] unn

-- | For variable foo: "collect(var_foo);"
collect :: String -> CStat
collect v = let
  in CExpr (Just $ CCall (var "collect") [var $ varName v] unn) unn

-- | Attaches a label to a list of statements. If the list is empty, creates
-- a null statement to attach the label to.
label :: String -> [CStat] -> [CStat]
label str stmts = case stmts of
  [] -> [addLabel $ CExpr Nothing unn]
  (x:xs) -> addLabel x : xs
  where addLabel stmt = CLabel (idt str) stmt [] unn

-- | Prints a string with @printf()@, then exits with status 0.
exitWith :: String -> [CStat]
exitWith str =
  [ CExpr (Just $ CCall (var "printf") [msg] unn) unn
  , CExpr (Just $ CCall (var "exit") [zero] unn) unn
  ] where zero = CConst $ CIntConst (cInteger 0) unn
          msg = CConst $ CStrConst (cString str) unn

-- | Generates statements for a single basic block.
block :: Go (Posn, Direction) (Maybe String) Command -> [CStat]
block g = case g of
  -- Node: call command function
  v :>> x -> command v ++ block x
  -- Branch: if statement
  x :|| y -> let
    ifT = CCompound [] (map CBlockStmt $ block y) unn
    ifF = CCompound [] (map CBlockStmt $ block x) unn
    in [CIf (var "condition") ifT (Just ifF) unn]
  -- Continue: goto a label
  Continue c -> [CGoto (idt $ makeLabel c) unn]
  -- Successful end: return from function
  End Nothing -> [CReturn Nothing unn]
  -- Crash end: print message and exit()
  End (Just s) -> exitWith s

-- | Produces statements to execute a single command.
command :: Command -> [CStat]
command c = case c of
  -- Pushing from var foo becomes: "push(var_foo);"
  Push v -> [CExpr (Just $ CCall (var "push") [var $ varName v] unn) unn]
  -- Popping from var foo becomes:
  -- "collect(var_foo); var_foo = pop();"
  -- Then, at the end of the function: "collect(var_foo)"
  Pop _ -> []
  -- A call to function foo becomes: "fun_foo();"
  Call f -> [call $ funName f]
  Val v -> case v of
    _ -> undefined
  -- A use of builtin add becomes: "builtin_add();"
  _ -> [call $ "builtin_" ++ map toLower (show c)]

val :: Val -> CStat
val = undefined

call :: String -> CStat
call str = CExpr (Just $ CCall (var str) [] unn) unn
