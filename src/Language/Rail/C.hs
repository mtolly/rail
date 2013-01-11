-- | A Rail to C compiler. Generates a single file of C99-compliant code.
module Language.Rail.C where

import Data.ControlFlow
import Language.Rail.Base
import Paths_rail (getDataFileName)
import Data.Char (toLower, isAscii, isAlphaNum, isPrint)
import qualified Data.Map as Map
import Data.List (nub, intersperse)
import Text.PrettyPrint.HughesPJ (Doc, text, hcat, vcat, render, nest, ($$))
import Numeric (showHex)

-- | The definitions of all the built-in functions and memory operations.
header :: IO String
header = getDataFileName "header.c" >>= readFile

-- | The main function, which simply calls @fun_main()@ (the Rail @main@
-- function).
footer :: String
footer = "\n\nint main() { fun_main(); return 0; }\n"

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

-- | Turns @((4, 7), NE)@ into @\"NE_4_7\"@.
makeLabel :: (Posn, Direction) -> String
makeLabel ((r, c), d) = show d ++ "_" ++ show r ++ "_" ++ show c

-- | Produces a C99 string literal.
stringLit :: String -> String
stringLit s = "\"" ++ concatMap f s ++ "\"" where
  f '"' = "\\\""
  f c | elem c "\\\a\b\f\n\r\t\v" = take 2 $ drop 1 $ show c
      | isAscii c && isPrint c      = [c]
      | otherwise                   = let
        hex = showHex (fromEnum c) ""
        in case length hex of
          len | len <= 4  -> "\\u" ++ replicate (4 - len) '0' ++ hex
              | len <= 8  -> "\\U" ++ replicate (8 - len) '0' ++ hex
              | otherwise -> error "stringLit: char out of range"

makeCFile :: [(String, System' (Posn, Direction))] -> IO String
makeCFile pairs = do
  h <- header
  let funs = render $ makeProgram pairs
  return $ h ++ funs ++ footer

makeProgram :: [(String, System' (Posn, Direction))] -> Doc
makeProgram pairs = let
  makeForward fname = text $ "void " ++ funName fname ++ "();"
  in vcat
    [ vcat $ map (makeForward . fst) pairs
    , text ""
    , vcat $ intersperse (text "") $ map (uncurry makeFunction) pairs ]

-- | Translates a Rail function to a C function.
makeFunction :: String -> System' (Posn, Direction) -> Doc
makeFunction fname sys = let
  vars = variables sys
  startCode = block $ systemStart sys
  pathsCode = map (\(pd, path) -> label (makeLabel pd) (block path)) $
    Map.toList $ systemPaths sys
  cleanup = concatMap (\v -> [removeRef v, collect v]) vars
  in vcat
    [ text $ "void " ++ funName fname ++ "() {" -- function declaration
    , nest 2 $ vcat
      [ vcat $ map vardecl vars -- local variable declarations
      , startCode -- entry point code
      , vcat pathsCode -- each labeled path's code
      , label "done" $ vcat cleanup $$ text "return;"-- detach/collect locals
      ]
    , text "}" ]

-- | Find all the local variables used in a function.
variables :: System' (Posn, Direction) -> [String]
variables (System st ps) = nub $ concatMap pathVars (st : Map.elems ps) where
  pathVars :: Go (Posn, Direction) Result Command -> [String]
  pathVars g = case g of
    Push v :>> x -> v : pathVars x
    Pop  v :>> x -> v : pathVars x
    _      :>> x -> pathVars x
    x      :|| y -> pathVars x ++ pathVars y
    _            -> []

-- | For variable foo, make the declaration @\"struct value *var_foo = NULL;@\"
vardecl :: String -> Doc
vardecl v = text $ "struct value *" ++ varName v ++ " = NULL;"

-- | Attaches a label to a (non-empty) list of statements.
label :: String -> Doc -> Doc
label str stmts = vcat
  [ nest (-2) $ text $ str ++ ":"
  , stmts ]

-- | Prints a string with @printf()@, then exits with status @0@.
exitWith :: String -> Doc
exitWith s = vcat
  [ text $ "printf(" ++ stringLit s ++ ");"
  , text "exit(0);" ]

-- | Generates statements for a single basic block.
block :: Go (Posn, Direction) Result Command -> Doc
block g = case g of
  -- Node: call command function
  v :>> x -> command v $$ block x
  -- Branch: if statement
  x :|| y -> vcat
    [ text "if (pop_bool()) {"
    , nest 2 $ block y
    , text "} else {"
    , nest 2 $ block x
    , text "}"]
  -- Continue: goto a label
  Continue pd -> text $ "goto " ++ makeLabel pd ++ ";"
  -- End: succesful end of function, or crash
  End e -> case e of
    Return -> text "goto done;"
    Internal s -> exitWith s
    Boom -> text "builtin_boom();"

-- | Produces statements to execute a single command.
command :: Command -> Doc
command c = case c of
  Push v -> text $ "push(" ++ varName v ++ ");"
  Pop v -> text $ "pop_to_var(&" ++ varName v ++ ");"
  Call f -> text $ funName f ++ "();"
  Val v -> hcat [text "push(", generate v, text ");"]
  _ -> text $ "builtin_" ++ map toLower (show c) ++ "();"

-- | For variable foo: @\"remove_reference(var_foo);@\"
removeRef :: String -> Doc
removeRef v = text $ "remove_reference(" ++ varName v ++ ");"

-- | For variable foo: @\"collect(var_foo);@\"
collect :: String -> Doc
collect v = text $ "collect(" ++ varName v ++ ");"

-- | An expression that generates @\@struct value *foo@\@.
generate :: Val -> Doc
generate v = case v of
  Str s -> text $ "new_str_copy(" ++ stringLit s ++ ")"
  Nil -> text "new_nil()"
  Pair x y ->
    hcat [text "make_pair(", generate x, text ", ", generate y, text ")"]
