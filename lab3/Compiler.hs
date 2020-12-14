-- Optional: turn on warnings.
-- {-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

-- | Compiler for C--, producing symbolic JVM assembler.

module Compiler where

import Control.Monad
--import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.RWS

import Data.Maybe
import qualified Data.Map as Map
import Data.List ( intercalate )

import Annotated as A
import CMM.Abs
import TypeChecker (getArgId,  getArgType )

type Env = ([(Id, String)], [(Id, Int)], Int, Int)
-- (funTypes, varAdresses, varCounter, jumpCounter)

--Thus the environment contains
-- • for each function, its type in the JVM notation; 
-- • for each variable, its address as an integer;
-- • a counter for variable addresses;
-- • a counter for jump labels

-- | Entry point.

compile
  :: String  -- ^ Class name.
  -> A.Program -- ^ Type-annotated program.
  -> String  -- ^ Generated jasmin source file content.
compile name _prg = header ++ "\n" ++ body (makeEnv name _prg) _prg
  where
  header :: String
  header = unlines $ concat
    [ [ ";; BEGIN HEADER"
      , ""
      , ".class public " ++ name
      , ".super java/lang/Object"
      , ""
      , ".method public <init>()V"
      , ".limit locals 1"
      , ".limit stack 1"
      , ""
      ]
    , map indent
      [ "aload_0"
      , "invokespecial java/lang/Object/<init>()V"
      , "return"
      ]
    , [ ""
      , ".end method"
      , ""
      , ".method public static main([Ljava/lang/String;)V"
      , ".limit locals 1"
      , ".limit stack  1"
      , ""
      ]
    , map indent
      [ "invokestatic " ++ name ++ "/main()I"
      , "pop"
      , "return"
      ]
    , [ ""
      , ".end method"
      , ""
      , ";; END HEADER"
      ]
    ]

body :: Env -> A.Program -> String
body _ (A.PDefs []) = ""
body env@(funs, vars, varC, _) (A.PDefs (def:defs)) = funCode ++ body (funs, vars, varC, jumpC) (A.PDefs defs)
  where 
    (funCode, (_, _, _, jumpC)) = function env def

-- DFun A.Type A.Id [A.Arg] [Stm]
function :: Env -> A.Def -> (String, Env)
function env@(funs, _, _, jmpC) (A.DFun typ id args stms) = (unlines $ concat
  [ [".method public static " ++ getSignature "" (A.DFun typ id args stms)],
    [".limit locals " ++ show newVarC],
    [".limit stack " ++ "200"],
    reverse $ map indent code, 
    [return],
    [".end method"]
  ], newEnv)
  where 
    (code, newEnv@(_, _, newVarC, _)) = foldl statement ([""], (funs, vars, varC, jmpC)) stms
    varC = sum $ map (size . getArgType) args
    vars = zip (map getArgId args) [0..]
    return = case typ of
      Type_void -> "return"
      _ -> ""

size :: Type -> Int
size Type_double = 2
size Type_int = 1
size Type_bool = 1
size Type_void = 0

statement :: ([String], Env) -> A.Stm -> ([String], Env)
statement (code, env@(funs, vars, varC, jmpC)) stmt = 
  case stmt of 
  (A.SExp exp@(_, typ)) -> case typ of
    Type_void -> (newCode, expEnv)
    Type_double -> ("pop2": newCode, expEnv)
    _ -> ("pop": newCode, expEnv)
    where 
      (newCode, expEnv) = expression (code, env) exp  
  (A.SDecls typ [id]) -> (newCode, newEnv)
    where
      newCode = (";; declare " ++ show id ++ " variable"):code
      newEnv = (funs, (id, varC):vars, varC + size typ, jmpC)
  (A.SInit typ id exp) -> (store typ varC:newCode, expEnv)
    where
      newEnv = (funs, (id, varC):vars, varC + size typ, jmpC)
      (newCode, expEnv) = expression (code, newEnv) exp
  (A.SReturn (e, typ)) -> (ret typ:newCode, expEnv)
    where
      (newCode, expEnv) = expression (code, env) (e, typ)
  (A.SWhile exp stmt) -> (whileCode ++ code, (funs, vars, newVarC, newjmpC + 2))
    where
       (condCode, condEnv) = expression ([], env) exp
       (stmtCode, (_, _, newVarC, newjmpC)) = statement ([], condEnv) stmt
       condLabel = label (newjmpC + 1)
       blockLabel = label newjmpC
       whileCode = reverse $ concat [
         ["goto " ++ condLabel],
         [blockLabel ++ ":"],
         reverse stmtCode, 
         [condLabel ++ ":"], 
         reverse condCode,
         ["ifgt " ++ blockLabel]
        ]
  (A.SBlock stmts) -> (newCode, (funs, vars, newVarC, newjmpC))
    where
      (newCode, (_, _, newVarC, newjmpC)) = foldl statement (code, env) stmts
  (A.SIfElse cond thenStmt elseStmt) -> (ifElseCode ++ code, (funs, vars, newVarC, newjmpC + 2))
    where
      (condCode, condEnv) = expression ([], env) cond
      (thenCode, thenEnv) = statement ([], condEnv) thenStmt
      (elseCode, (_, _, newVarC, newjmpC)) = statement ([], thenEnv) elseStmt
      elseLabel = label newjmpC
      endLabel = label (newjmpC + 1)
      ifElseCode = reverse $ concat [
        reverse condCode,
        ["ifeq " ++ elseLabel],
        reverse thenCode,
        ["goto " ++ endLabel],
        [elseLabel ++ ":"],
        reverse elseCode,
        [endLabel ++ ":"],
        ["nop"]
        ]

label :: Int -> String
label i = "L" ++ show i

expression :: ([String], Env) -> A.Exp -> ([String], Env)
expression (code, env@(funs, vars, _, _)) exp =
  case exp of 
    (A.EBool LTrue, _) -> ("iconst_1":code, env)
    (A.EBool LFalse, _) -> ("iconst_0":code, env)
    (A.EInt 0, _) -> ("iconst_0":code, env)
    (A.EInt 1, _) -> ("iconst_1":code, env)
    (A.EInt 2, _) -> ("iconst_2":code, env)
    (A.EInt 3, _) -> ("iconst_3":code, env)
    (A.EInt 4, _) -> ("iconst_4":code, env)
    (A.EInt 5, _) -> ("iconst_5":code, env)
    (A.EInt i, _) -> (("sipush " ++ show i):code, env)
    (A.EDouble 0.0, _) -> ("dconst_0":code, env)
    (A.EDouble 1.0, _) -> ("dconst_1":code, env)
    (A.EDouble d, _) -> (("ldc2_w " ++ show d):code, env)
    (A.EId id, typ) -> case lookup id vars of
      Just vNum -> ((typeChar typ ++ "load " ++ show vNum ):code, env)
    (A.EApp id args, _) -> case lookup id funs of 
      Just sign -> (("invokestatic " ++ sign):argCode, newEnv)
        where (argCode, newEnv) = foldl expression (code, env) args
    (A.EPost id op, typ) -> (inc op typ varAdr ++ [loadOp] ++ code, env)
      where
        loadOp = typeChar typ ++ "load " ++ show varAdr
        varAdr = case lookup id vars of
          Just adr -> adr
    (A.EPre op id, typ) -> ([loadOp] ++ inc op typ varAdr ++ code, env)
      where
        loadOp = typeChar typ ++ "load " ++ show varAdr
        varAdr = case lookup id vars of
          Just adr -> adr
    (A.EMul exp1 mulOp exp2, typ) -> (mul typ mulOp:expCode, newEnv)
      where 
        (expCode, newEnv) = foldl expression (code, env) [exp1, exp2]
    (A.EAdd exp1 addOp exp2, typ) -> (add typ addOp:expCode, newEnv)
      where 
        (expCode, newEnv) = foldl expression (code, env) [exp1, exp2]
    (A.ECmp exp1 cmpOp exp2, typ) -> (cmpCode ++ convertType:expCode2, (f, v, vC, jC + 2))
      where 
        (expCode1, newEnv) = expression (code, env) exp1
        (expCode2, (f, v, vC, jC)) = expression (convertType:expCode1, newEnv) exp2
        convertType = case typ of 
          Type_int -> "i2l"
          Type_bool -> "i2l"
          _ -> ""
        cmpCode = reverse [
          cmp typ,
          op ++ " L" ++ show jC,
          "iconst_0",
          "goto L" ++ show (jC + 1),
          "L" ++ show jC ++ ":",
          "iconst_1",
          "L" ++ show (jC + 1) ++ ":"
          ]
        op = case cmpOp of 
          OLt -> "iflt"
          OGt -> "ifgt"
          OLtEq -> "ifle"
          OGtEq -> "ifge"
          OEq -> "ifeq"
          ONEq -> "ifne"
    (A.EAnd exp1 exp2, _) -> (andCode ++ expCode2, (f2, v2, vC2, jC2 + 2))
      where 
        (expCode1, newEnv) = expression (code, env) exp1
        (expCode2, (f2, v2, vC2, jC2)) = expression (("ifeq L" ++ show jC2):expCode1, newEnv) exp2
        andCode = reverse [
          "ifeq L" ++ show jC2,
          "iconst_1",
          "goto L" ++ show (jC2 + 1),
          "L" ++ show jC2  ++ ":",
          "iconst_0",
          "L" ++ show (jC2 + 1) ++ ":"
          ]
    (A.EOr exp1 exp2, _) -> (orCode ++ expCode2, (f2, v2, vC2, jC2 + 2))
      where 
        (expCode1, newEnv) = expression (code, env) exp1
        (expCode2, (f2, v2, vC2, jC2)) = expression (("ifgt L" ++ show jC2):expCode1, newEnv) exp2
        orCode = reverse [
          "ifgt L" ++ show jC2,
          "iconst_0",
          "goto L" ++ show (jC2 + 1),
          "L" ++ show jC2  ++ ":",
          "iconst_1",
          "L" ++ show (jC2 + 1) ++ ":"
          ]
    (A.EAss id exp, typ) -> ([typeChar typ ++ "store " ++ show varAdr, dup typ] ++ expCode, newEnv)
      where
        (expCode, newEnv) = expression (code, env) exp
        varAdr = case lookup id vars of
          Just adr -> adr

incDecInt :: IncDecOp -> String
incDecInt OInc = "1"
incDecInt ODec = "-1"

dup :: Type -> String
dup Type_double = "dup2"
dup _ = "dup"

inc :: IncDecOp -> Type -> Int -> [String]
inc op Type_int varAdr = ["iinc " ++ show varAdr ++ " " ++ incDecInt op]
inc op Type_double varAdr = reverse [
  "dload " ++ show varAdr,
  "ldc2_w " ++ incDecInt op ++ ".0", 
  "dadd",
  "dstore " ++ show varAdr
  ]

mul :: Type -> MulOp -> String
mul t OTimes = typeChar t ++ "mul"
mul t ODiv = typeChar t ++ "div"

add :: Type -> AddOp -> String
add t OPlus = typeChar t ++ "add"
add t OMinus = typeChar t ++ "sub"

cmp :: Type -> String
cmp Type_int = "lcmp"
cmp Type_bool = "lcmp"
cmp Type_double = "dcmpg"

store :: Type -> Int -> String
store typ adr = typeChar typ ++ "store " ++ show adr

ret :: Type -> String
ret typ = typeChar typ ++ "return"

makeEnv :: String -> A.Program -> Env
makeEnv name (A.PDefs defs) = (funs, [], 0, 0)
  where
    funs = zip (map (\(A.DFun _ id _ _) -> id) defs) (map (getSignature name) defs) ++ 
      [(Id "readInt", "Runtime/readInt()I"), 
      (Id "readDouble", "Runtime/readDouble()D"), 
      (Id "printInt", "Runtime/printInt(I)V"),
      (Id "printDouble", "Runtime/printDouble(D)V"),
      (Id "double", "Runtime/toDouble(I)D")]

getSignature :: String -> A.Def -> String
getSignature "" (A.DFun typ (Id id) args _) = id ++ "(" ++ concatMap (typeFlag . getArgType) args ++ ")" ++ typeFlag typ
getSignature name (A.DFun typ (Id id) args _) = name ++ "/" ++ id ++ "(" ++ concatMap (typeFlag . getArgType) args ++ ")" ++ typeFlag typ

typeChar :: Type -> String
typeChar Type_double = "d"
typeChar Type_void = ""
typeChar _ = "i"

typeFlag :: Type -> String
typeFlag Type_bool = "Z"
typeFlag Type_int = "I"
typeFlag Type_double = "D"
typeFlag Type_void = "V"

-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s then s else "\t" ++ s
