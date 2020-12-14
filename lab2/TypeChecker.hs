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
typecheck :: Program -> Err A.Program
typecheck (PDefs []) = Bad "Empty program"
typecheck (PDefs defs) | not (elem (Id "main") (map getFunId defs)) = Bad "Empty program"
typecheck (PDefs defs) = do
    env <- extendFun emptyEnv defs
    checkFun [] env defs

checkFun :: [A.Def] -> Env -> [Def] -> Err A.Program
checkFun aStms _ [] = Ok (A.PDefs aStms)
checkFun aStms env ((DFun fType id args stmts):defs) = do
  newEnv <- extendVar env args
  case checkStms fType (newEnv, []) stmts of
    Ok (_, aastmts) -> checkFun (aStms ++ [A.DFun fType id args aastmts]) env defs
    Bad s -> Bad s

checkStms :: Type -> (Env, [A.Stm]) -> [Stm] -> Err (Env, [A.Stm])
checkStms fType = foldM (checkStm fType)


checkStm :: Type -> (Env, [A.Stm]) -> Stm -> Err (Env, [A.Stm])
checkStm fType (env, aStms) stm = case stm of
  SExp exp -> do 
    (_, e) <- inferExp env exp 
    return (env, aStms ++ [A.SExp e])
  SDecls _ [] -> Ok (env, aStms)
  SDecls varType (id:ids) -> do 
    newEnv <- updateVar env id varType
    checkStm fType (newEnv, aStms ++ [A.SDecls varType [id]]) (SDecls varType ids)
  SInit varType id exp -> do 
    (newEnv, newAStms) <- checkStm fType (env, aStms) (SDecls varType [id])
    (typ, newExp) <- inferExp newEnv exp
    cExp <- convertExpression varType typ newExp
    Ok (newEnv, newAStms ++ [A.SExp (A.EAss id cExp, typ)])
  SReturn exp -> do
    (typ, newExp) <- inferExp env exp
    cExp <- convertExpression fType typ newExp
    Ok (env, aStms ++ [A.SReturn cExp])
  SWhile exp block -> do
    (conditionType, e) <- inferExp env exp
    if conditionType /= Type_bool 
      then 
        Bad "Condition in while statement must be Bool"
      else do   
        (_, stms) <- checkStm fType (env, []) block
        Ok (env, aStms ++ [A.SWhile e (head stms)])
  SBlock stms -> case checkStms fType (newBlock env, []) stms of
    Ok (_, blockStms) -> return (env, aStms ++ [A.SBlock blockStms])
    e -> e
  SIfElse exp stm1 stm2 -> do
    (conditionType, e) <- inferExp env exp
    if conditionType /= Type_bool 
      then 
        Bad "Condition in if statement must be Bool"
      else do   
        (_, block1) <- checkStm fType (newBlock env, []) stm1
        (_, block2) <- checkStm fType (newBlock env, []) stm2
        Ok (env, aStms ++ [A.SIfElse e (head block1) (head block2)])

-- Checks that Expected type are equal or larger than actual type
-- Makes an explicit type conversion on expression if expected type is larger than actual
convertExpression :: Type -> Type -> A.Exp -> Err A.Exp
convertExpression Type_double Type_int exp = Ok (double exp)
convertExpression expected actual exp | expected == actual = Ok exp
convertExpression expected actual exp = Bad ("Incompatible types " ++ show expected ++ ", " ++ show actual ++ ": " ++ show exp)

double :: A.Exp -> A.Exp
double aExp = (A.EApp (Id "double") [aExp], Type_double)

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

getArgId :: Arg -> Id
getArgId (ADecl _ id) = id

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

inferExp :: Env -> Exp -> Err (Type, A.Exp) 
inferExp env x = case x of
    EBool b -> Ok (Type_bool, (A.EBool b, Type_bool))
    EInt i -> Ok (Type_int, (A.EInt i, Type_int))
    EDouble d -> Ok (Type_double, (A.EDouble d, Type_double))
    EId id -> do
      typ <- lookupVar env id
      Ok (typ, (A.EId id, typ)) 
    EAdd exp1 op exp2 -> do
      (typ, e1, e2) <- inferNumeric env exp1 exp2
      Ok (typ, (A.EAdd e1 op e2, typ))
    EMul exp1 op exp2 -> do 
      (typ, e1, e2) <- inferNumeric env exp1 exp2
      Ok (typ, (A.EMul e1 op e2, typ))
    EPost id op -> do
      typ <- inferVar [Type_double, Type_int] env id
      Ok (typ, (A.EPost id op, typ))
    EPre op id -> do 
      typ <- inferVar [Type_double, Type_int] env id
      Ok (typ, (A.EPre op id, typ))
    ECmp exp1 op exp2 | op == OEq || op == ONEq -> case inferNumeric env exp1 exp2 of
      Ok (opType, e1, e2) -> Ok (Type_bool, (A.ECmp e1 op e2, opType))
      _ -> case inferBool env exp1 exp2 of
        Ok (opType, e1, e2) -> return (Type_bool, (A.ECmp e1 op e2, opType))
    ECmp exp1 op exp2 -> case inferNumeric env exp1 exp2 of
      Ok (opType, e1, e2) -> return (Type_bool, (A.ECmp e1 op e2, opType))
    EAnd exp1 exp2 -> do 
      (opType, e1, e2) <- inferBool env exp1 exp2
      Ok (Type_bool, (A.EAnd e1 e2, opType))
    EOr exp1 exp2 -> do
      (opType, e1, e2) <- inferBool env exp1 exp2
      Ok (Type_bool, (A.EOr e1 e2, opType))  
    EAss id exp -> do
      expectedType <- lookupVar env id
      (actualType, newExp) <- inferExp env exp
      cExp <- convertExpression expectedType actualType newExp
      Ok (actualType, (A.EAss id cExp, actualType))
    EApp id args -> do 
        (argTypes, returnType) <- lookupFun env id
        aArgs <- mapRight (inferExp env) args
        if length argTypes /= length args 
          then Bad "Incorrect number of function arguments"
          else
            if all contains (zip (map fst aArgs) (map smallerTypes argTypes))
              then
                Ok (returnType, (A.EApp id (map snd aArgs), Type_void))
              else
                Bad ("wrong type of function. Expected " ++ show argTypes ++ " but got " ++ show (map fst aArgs))

contains :: (Type, [Type]) -> Bool
contains (a, b) = elem a b

smallerTypes :: Type -> [Type]
smallerTypes Type_double = [Type_int, Type_double]
smallerTypes a = [a]

-- checks wether the variable is of allowed type and returns its type
inferVar :: [Type] -> Env -> Id -> Err Type
inferVar types env id = do
    (typ, _) <- inferExp env (EId id)
    if elem typ types
      then
        Ok typ
      else
        Bad ("wrong type of variable " ++ printTree (EId id))

-- checks wether the expressions are of allowed types and returns annotated expressions
-- where operands are typed and coersions made explicit
inferNumeric :: Env -> Exp -> Exp -> Err (Type, A.Exp, A.Exp) 
inferNumeric env exp1 exp2 = do
    (typ1, (e1, opTyp1)) <- inferExp env exp1 
    (typ2, (e2, opTyp2)) <- inferExp env exp2
    case (typ1, typ2) of
      (Type_bool, Type_bool) -> Bad "Incompatible types"  
      (a, b) | a == b -> Ok (typ1, (e1, opTyp1), (e2, opTyp2))
      (Type_double, Type_int) -> Ok (typ1, (e1, opTyp1), double (e2, opTyp2))
      (Type_int, Type_double) -> Ok (typ1, double (e1, opTyp1), (e2, opTyp2))
      _ -> Bad "Incompatible types"

inferBool :: Env -> Exp -> Exp -> Err (Type, A.Exp, A.Exp) 
inferBool env exp1 exp2 = do
    (typ1, (e1, opTyp1)) <- inferExp env exp1 
    (typ2, (e2, opTyp2)) <- inferExp env exp2
    case (typ1, typ2) of
      (Type_bool, Type_bool) -> Ok (typ1, (e1, opTyp1), (e2, opTyp2))
      _ -> Bad "Incompatible types"

checkExp :: Env -> Exp -> Type -> Err Type
checkExp env exp typ = do
  (t, _) <- inferExp env exp 
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
