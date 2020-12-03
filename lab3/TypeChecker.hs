{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TupleSections    #-}

-- | Type checker for C--, producing typed syntax from ASTs.

module TypeChecker where

import Control.Applicative
import Control.Monad
--import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.State

import Data.Functor
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

import CMM.Abs
import CMM.Print (printTree)
import CMM.ErrM
import Data.Either

import qualified Annotated as A

type TypeError = String

-- | Entry point of type checker.

type Env = (Sig,[Context])
type Sig = [(Id, ([Type], Type))]
type Context = [(Id, Type)]

-- check a whole program
typecheck :: Program -> Either TypeError A.Program
typecheck (PDefs []) = Bad "Empty program"
typecheck (PDefs defs) | not (elem (Id "main") (map getFunId defs)) = Bad "Empty program"
typecheck (PDefs defs) = do
    env <- extendFun emptyEnv defs
    checkFun env defs

checkFun :: Env -> [Def] -> Either TypeError A.Program
checkFun _ [] = Ok ()
checkFun env ((DFun fType id args stmts):defs) = do
  newEnv <- extendVar env args
  case checkStms fType newEnv stmts of
    Ok _ -> checkFun env defs
    Bad s -> Bad s

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms fType = foldM (checkStm fType)

checkStm :: Type -> Env -> Stm -> Err Env 
checkStm fType env stm = case stm of
  SExp exp -> do 
    inferExp env exp 
    return env
  SDecls _ [] -> Ok env 
  SDecls varType (id:ids) -> do 
    newEnv <- updateVar env id varType
    checkStm fType newEnv (SDecls varType ids)
  SInit varType id exp -> do 
    env1 <- checkStm fType env (SDecls varType [id])
    expType <- inferExp env1 exp
    case maxType expType varType of 
      Ok typ | varType == typ -> Ok env1
      _ -> Bad "Wrong type in variable initialization"
  SReturn exp -> do
    expType <- inferExp env exp
    case maxType expType fType of
      Ok typ | typ == fType -> Ok env
      _ -> Bad "Wrong return type in function"
  SWhile exp stm -> do 
    checkExp env exp Type_bool
    checkStm fType env (SBlock [stm])
  SBlock stms -> case checkStms fType (newBlock env) stms of
    Ok _ -> return env
    e -> e
  SIfElse exp stm1 stm2 -> do
    cond <- inferExp env exp
    case cond of
      Type_bool -> checkStms fType env [SBlock [stm1], SBlock [stm2]]
      _ -> Bad "Condition not of type Bool"

extendVar :: Env -> [Arg] -> Err Env
extendVar env [] = Ok env
extendVar _ ((ADecl Type_void _):_) = Bad "Argument can't be of type void"
extendVar env ((ADecl typ id):args) = case updateVar env id typ of
  Ok newenv -> extendVar newenv args
  e -> e

extendFun :: Env -> [Def] -> Err Env
extendFun env [] = Ok env
extendFun env ((DFun typ id args _):defs) = case updateFun env id (map getArgType args, typ) of
  Ok extenv -> extendFun extenv defs
  e -> e

getArgType :: Arg -> Type
getArgType (ADecl typ _) = typ 

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) _ = Bad "Variable not declared"
lookupVar (sig, block:blocks) id = case lookup id block of
  Just varType -> Ok varType
  Nothing -> lookupVar (sig, blocks) id

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun (sig, _) id = case lookup id sig of
  Just funType -> Ok funType
  Nothing -> Bad ("Undefined function :" ++ show id)

updateVar :: Env -> Id -> Type -> Err Env
updateVar _ _ Type_void = Bad "Variable can't be of type void"
updateVar (sig, block:blocks) id typ = do
  if elem id $ map fst block
    then
      Bad ("Variable " ++ show id ++ " already defined in block.")
    else
      Ok (sig, ((id, typ):block):blocks)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (_, _) (Id "main") (args, ret) | not (null args) || ret /= Type_int = Bad "incorrect main function"
updateFun (sig, context) id funType = case lookup id sig of
  Just _ -> Bad ("Function " ++ show id ++ " already declared")
  Nothing -> Ok ((id, funType):sig, context)

newBlock :: Env -> Env
newBlock (sig, blocks) = (sig, []:blocks)

--    void   printInt(int x)        // print an integer and a newline in standard output
--    void   printDouble(double x)  // print a double and a newline in standard output
--    int    readInt()              // read an integer from standard input
--    double readDouble()           // read a double from standard input
emptyEnv :: Env
emptyEnv = (predefinedFunctions,[[]])

predefinedFunctions :: Sig
predefinedFunctions = [(Id "printInt", ([Type_int], Type_void)), (Id "printDouble", ([Type_double], Type_void)), (Id "readInt", ([], Type_int)), (Id "readDouble", ([], Type_double))]

inferExp :: Env -> Exp -> Err Type 
inferExp env x = case x of
    EBool _ -> return Type_bool 
    EInt _ -> return Type_int 
    EDouble _ -> return Type_double
    EId id -> lookupVar env id 
    EAdd exp1 _ exp2 -> inferBin [Type_int, Type_double] env exp1 exp2
    EMul exp1 _ exp2 -> inferBin [Type_int, Type_double] env exp1 exp2
    EPost id _ -> inferUn [Type_double, Type_int] env (EId id)
    EPre _ id -> inferUn [Type_double, Type_int] env (EId id)
    ECmp exp1 op exp2 | op == OEq || op == ONEq -> case inferBin [Type_int, Type_double] env exp1 exp2 of
      Ok _ -> return Type_bool
      _ -> case inferBin [ Type_bool] env exp1 exp2 of
        Ok _ -> return Type_bool
        e -> e
    ECmp exp1 _ exp2 -> case inferBin [Type_int, Type_double] env exp1 exp2 of
      Ok _ -> return Type_bool
      e -> e
    EAnd exp1 exp2 -> inferBin [Type_bool] env exp1 exp2
    EOr exp1 exp2 -> inferBin [Type_bool] env exp1 exp2
    EAss id exp -> case lookupVar env id of
        Ok typ -> inferUn (smallerTypes typ) env exp
        e -> e
    EApp id args -> do 
        (argTypes, returnType) <- lookupFun env id
        passedTypes <- mapRight (inferExp env) args
        if length argTypes /= length args 
          then Bad "Incorrect number of function arguments"
          else
            if all contains (zip passedTypes (map smallerTypes argTypes))
              then
                Ok returnType
              else
                Bad ("wrong type of function. Expected " ++ show argTypes ++ " but got " ++ show passedTypes)

contains :: (Type, [Type]) -> Bool
contains (a, b) = elem a b

smallerTypes :: Type -> [Type]
smallerTypes Type_double = [Type_int, Type_double]
smallerTypes a = [a]

inferUn :: [Type] -> Env -> Exp -> Err Type 
inferUn types env exp = do
    typ <- inferExp env exp
    if elem typ types
      then
        Ok typ
      else
        Bad ("wrong type of expression " ++ printTree exp)

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type 
inferBin types env exp1 exp2 = do
    typ1 <- inferExp env exp1 
    typ2 <- inferExp env exp2 
    case maxType typ1 typ2 of 
      Ok typ | elem typ1 types && elem typ2 types -> Ok typ
      _ -> Bad ("wrong type of expression " ++ printTree exp1)

maxType :: Type -> Type -> Err Type
maxType Type_int Type_double = Ok Type_double
maxType Type_double Type_int = Ok Type_double
maxType a b | a == b = Ok a 
maxType _ _ = Bad "Incompatible types"

checkExp :: Env -> Exp -> Type -> Err Type
checkExp env exp typ = do
  t <- inferExp env exp 
  if t == typ
    then
      Ok typ
    else 
      Bad ("wrong type of expression " ++ printTree exp ++" actual type: " ++ show t ++ " expected type: " ++ show typ)

-- Help functions

getFunId :: Def -> Id
getFunId (DFun _ id _ _) = id

mapErr :: (a -> Err b) -> [a] -> Err ()
mapErr f l = do 
  if null (lefts (map f l))
    then
      Ok ()
    else
      Bad (head (lefts (map f l)))

mapRight :: (a -> Err b) -> [a] -> Err [b]
mapRight f l = do 
  if null (lefts (map f l))
    then
      Ok (rights (map f l))
    else
      Bad (head (lefts (map f l)))
