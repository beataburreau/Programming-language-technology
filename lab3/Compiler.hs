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
import CMM.Abs ( Id(Id), Type(..) )
import TypeChecker ( getArgType )

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
compile name _prg = header ++ "\n" ++ body (makeEnv _prg) _prg
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
body env (A.PDefs (def:defs)) = function env def ++ body env (A.PDefs defs)

-- DFun A.Type A.Id [A.Arg] [Stm]
function :: Env -> A.Def -> String
function env (A.DFun typ id args stms) = unlines $ concat
  [ [".method public static " ++ getSignature (A.DFun typ id args stms)],
    reverse $ map indent code, 
    [".end method"]
  ]
  where
    (code, _) = foldl statement ([""], env) stms

statement :: ([String], Env) -> A.Stm -> ([String], Env)
statement (code, env@(funs, vars, varC, jmpC)) stmt = 
  case stmt of 
  (A.SExp exp) -> ("pop": newCode, expEnv)
    where 
      (newCode, expEnv) = expression (code, env) exp  
  (A.SDecls _ [id]) -> (newCode, newEnv)
    where
      newCode = (";; declare " ++ show id ++ " variable"):code
      newEnv = (funs, (id, varC):vars, varC + 1, jmpC)
  (A.SInit typ id exp) -> (store typ varC:newCode, expEnv)
    where
      newEnv = (funs, (id, varC):vars, varC + 1, jmpC)
      (newCode, expEnv) = expression (code, newEnv) exp
  (A.SReturn (e, typ)) -> (ret typ:newCode, expEnv)
    where
      (newCode, expEnv) = expression (code, env) (e, typ)
  (A.SWhile exp stmt) -> (whileCode ++ code, (funs, vars, varC, newjmpC + 2))
    where
       (condCode, condEnv) = expression ([], env) exp
       (stmtCode, (_, _, _, newjmpC)) = statement ([], condEnv) stmt
       condLabel = label (newjmpC + 1)
       blockLabel = label newjmpC
       whileCode = reverse $ concat [
         ["goto " ++ condLabel, blockLabel ++ ":"],
         reverse stmtCode, 
         [condLabel ++ ":"], 
         reverse condCode, 
         ["iconst_1", "ifeq " ++ blockLabel]
        ]
  (A.SBlock stmts) -> (newCode, (funs, vars, varC, newjmpC))
    where
      (newCode, (_, _, _, newjmpC)) = foldl statement (code, env) stmts
  (A.SIfElse cond thenStmt elseStmt) -> (ifElseCode ++ code, (funs, vars, varC, newjmpC + 2))
    where
      (condCode, condEnv) = expression ([], env) cond
      (thenCode, thenEnv) = statement ([], condEnv) thenStmt
      (elseCode, (_, _, _, newjmpC)) = statement ([], thenEnv) elseStmt
      elseLabel = label newjmpC
      endLabel = label (newjmpC + 1)
      ifElseCode = reverse $ concat [
        reverse condCode,
        ["iconst_0", "ifeq " ++ elseLabel],
        reverse thenCode,
        ["goto " ++ endLabel,
        elseLabel ++ ":"],
        reverse elseCode,
        [endLabel ++ ":"]
        ]

label :: Int -> String
label i = "L" ++ show i

expression :: ([String], Env) -> Exp -> ([String], Env)
expression = undefined
-- type Env = ([(Id, String)], [(Id, Int)], Int, Int)
-- (funTypes, varAdresses, varCounter, jumpCounter)

store :: Type -> Int -> String
store Type_double adr = "dstore_" ++ show adr
store _ adr = "istore_" ++ show adr
-- bool: int (0 eller 1)

ret :: Type -> String
ret Type_double = "dreturn"
ret Type_void = "return"
ret _ = "ireturn"



makeEnv :: A.Program -> Env
makeEnv (A.PDefs defs) = (zip (map (\(A.DFun _ id _ _) -> id) defs) (map getSignature defs), [], 0, 0)

getSignature :: A.Def -> String
getSignature (A.DFun typ (Id id) args _) = id ++ "(" ++ intercalate ";" (map (typeFlag . getArgType) args) ++ ")" ++ typeFlag typ

typeFlag :: Type -> String
typeFlag Type_bool = "Z"
typeFlag Type_int = "I"
typeFlag Type_double = "D"
typeFlag Type_void = "V"

-- | Indent non-empty lines.
indent :: String -> String
indent s = if null s then s else "\t" ++ s
