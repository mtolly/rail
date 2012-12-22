-- | Rail to C compiler.
module Language.Rail.C where

import Data.ControlFlow
import Language.Rail.Base
import Paths_rail (getDataFileName)
import Data.Char (toLower, isAscii, isAlphaNum, isDigit)
import Language.C.Syntax
import Language.C.Data.Node
import Language.C.Data.Ident
import qualified Data.Map as Map
import qualified Data.Set as Set

header :: IO String
header = getDataFileName "header.c" >>= readFile

variables :: Function -> Set.Set String
variables = undefined

vardecl :: [String] -> CDecl
vardecl vs = let
  f v = (Just undefined, Nothing, Nothing)
  structValue =
    CStruct CStructTag (Just $ internalIdent "value") Nothing [] undefNode
  in CDecl [CTypeSpec $ CSUType structValue undefNode] (map f vs) undefNode

function :: Function -> [CStat]
function f = block (systemStart f) ++ concatMap g (Map.toList $ systemPaths f)
  where g (pd, go) = label (makeLabel pd) $ block go

label :: String -> [CStat] -> [CStat]
label str stmts = case stmts of
  [] -> [addLabel $ CExpr Nothing undefNode]
  (x:xs) -> addLabel x : xs
  where addLabel stmt = CLabel (builtinIdent str) stmt [] undefNode

makeLabel :: (Posn, Direction) -> String
makeLabel ((r, c), d) = show d ++ "_" ++ show r ++ "_" ++ show c

var :: String -> CExpr
var str = CVar (builtinIdent str) undefNode

exitWith :: String -> [CStat]
exitWith str =
  [ CExpr (Just $ CCall (var "printf") [msg] undefNode) undefNode
  , CExpr (Just $ CCall (var "exit") [zero] undefNode) undefNode
  ] where zero = CConst $ CIntConst (cInteger 0) undefNode
          msg = CConst $ CStrConst (cString str) undefNode

block :: Go (Posn, Direction) (Maybe String) Command -> [CStat]
block g = case g of
  v :>> x -> command v ++ block x
  x :|| y -> let
    ifT = CCompound [] (map CBlockStmt $ block y) undefNode
    ifF = CCompound [] (map CBlockStmt $ block x) undefNode
    in [CIf (var "condition") ifT (Just ifF) undefNode]
  Continue c -> [CGoto (builtinIdent $ makeLabel c) undefNode]
  End Nothing -> [CReturn Nothing undefNode]
  End (Just s) -> exitWith s

command :: Command -> [CStat]
command c = case c of
  Push _ -> []
  Pop _ -> []
  Call f -> [call $ "fun_" ++ mangle f]
  Val _ -> []
  _ -> [call $ "builtin_" ++ map toLower (show c)]

val :: Val -> CStat
val = undefined

call :: String -> CStat
call str = CExpr (Just $ CCall (var str) [] undefNode) undefNode

-- | Creates a valid suffix to a C identifier. It might start with underscores,
-- so for portability stick a prefix starting with a letter on the front.
mangle :: String -> String
mangle = concatMap $ \c ->
  if isAscii c && isAlphaNum c
    then [c]
    else if c == '_'
      then "__"
      else "_" ++ show (fromEnum c) ++ "_"

-- | @unmangle . mangle === id@
unmangle :: String -> String
unmangle "" = ""
unmangle ('_':'_':cs) = '_' : unmangle cs
unmangle ('_':cs) = case span isDigit cs of
  (dgts, '_':rest) -> toEnum (read dgts) : unmangle rest
  _ -> ""
unmangle (c:cs) = c : unmangle cs
