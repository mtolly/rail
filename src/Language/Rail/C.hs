-- | A Rail to C compiler. Generates a single file of C99-compliant code.
module Language.Rail.C
( makeFile
) where

import Data.Char (toLower, isAscii, isAlphaNum)
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map as Map
import Language.C.Syntax.Constants (showStringLit)
import Text.PrettyPrint.HughesPJ (Doc, text, hcat, vcat, render, nest, ($$))

import Data.ControlFlow
import Language.Rail.Base
import Language.Rail.Parse (Posn, Direction)
import Paths_rail (getDataFileName)

-- | The definitions of all the built-in functions and memory operations.
header :: String
header = unsafePerformIO $ getDataFileName "header.c" >>= readFile

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
stringLit s = showStringLit s ""

-- | Creates a single, complete C99 file, given a set of Rail functions.
makeFile :: [(String, System (Posn, Direction) () Result Command)] -> String
makeFile pairs = header ++ render (makeProgram pairs) ++ footer

makeProgram :: [(String, System (Posn, Direction) () Result Command)] -> Doc
makeProgram pairs = let
  makeForward fname = text $ "void " ++ funName fname ++ "();"
  in vcat
    [ vcat $ map (makeForward . fst) pairs
    , text ""
    , vcat $ intersperse (text "") $ map (uncurry makeFunction) pairs ]

-- | Translates a Rail function to a C function.
makeFunction :: String -> System (Posn, Direction) () Result Command -> Doc
makeFunction fname sys = let
  vars = systemVars sys
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
block :: Path (Posn, Direction) () Result Command -> Doc
block g = case g of
  -- Node: call command function
  v :>> x -> command v $$ block x
  -- Branch: if statement
  Branch () x y -> vcat
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
  Str s Nothing  -> text $ "new_str_copy(" ++ stringLit s ++ ")"
  Str _ (Just i) -> text $ "new_int(" ++ show i ++ ")"
  Nil -> text "new_nil()"
  Pair x y ->
    hcat [text "make_pair(", generate x, text ", ", generate y, text ")"]
