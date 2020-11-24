module TypeChecker where

import Control.Monad

import Data.Either

import Data.Bifunctor

import CMM.Abs

import CMM.Print
import CMM.ErrM

type Env = (Sig,[Context])
type Sig = [(Id, ([Type], Type))]
type Context = [(Id, Type)]

-- check a whole program
typecheck :: Program -> Err ()
typecheck (PDefs defs) = do
    env <- extendFun emptyEnv defs
    checkFun env defs

checkFun :: Env -> [Def] -> Err ()
checkFun _ [] = Ok ()
checkFun env ((DFun fType id args stmts):defs) = do
  newEnv <- extendVar env args
  case checkStms fType newEnv stmts of
    Ok _ -> checkFun env defs
    Bad s -> Bad s

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms fType = foldM (checkStm fType)

-- foldM :: Monad m => (Env -> Stm -> Err Env) -> Env -> [Stm] -> Err Env

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
    expType <- inferExp env exp
    if expType == varType
      then
        checkStm fType env (SDecls varType [id])
      else
        Bad "Wrong type in variable initialization"
  SReturn exp -> do
    expType <- inferExp env exp
    if expType == fType
      then
        Ok env
      else
        Bad "Wrong return type in function"
  SWhile exp stm -> do 
    checkExp env exp Type_bool
    checkStm fType env stm
  SBlock stms -> checkStms Type_void (newBlock env) stms
  SIfElse exp stm1 stm2 -> do
    cond <- inferExp env exp
    case cond of
      Type_bool -> checkStms fType env [SBlock [stm1], SBlock [stm2]]
      _ -> Bad "Condition not of type Bool"

extendVar :: Env -> [Arg] -> Err Env
extendVar env [] = Ok env
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
updateVar (sig, block:blocks) id typ = do
  if elem id $ map fst block
    then
      Bad ("Variable " ++ show id ++ " already defined in block.")
    else
      Ok (sig, ((id, typ):block):blocks)

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
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
emptyEnv = ([(Id "printInt", ([Type_int], Type_void)), (Id "printDouble", ([Type_double], Type_void)), (Id "readInt", ([], Type_int)), (Id "readDouble", ([], Type_double))],[[]])

inferExp :: Env -> Exp -> Err Type 
inferExp env x = case x of
    EBool _ -> return Type_bool 
    EInt _ -> return Type_int 
    EDouble _ -> return Type_double
    EId id -> lookupVar env id 
    EAdd exp1 _ exp2 -> inferBin [Type_int, Type_double] env exp1 exp2
    EMul exp1 _ exp2 -> inferBin [Type_int, Type_double] env exp1 exp2
    EPost id _ -> lookupVar env id
    EPre _ id -> lookupVar env id
    ECmp exp1 _ exp2 -> inferBin [Type_int, Type_double] env exp1 exp2
    EAnd exp1 exp2 -> inferBin [Type_bool] env exp1 exp2
    EOr exp1 exp2 -> inferBin [Type_bool] env exp1 exp2
    EAss id exp -> case lookupVar env id of
        Ok typ -> inferBin [typ] env exp exp
        e -> e
    EApp id args -> do 
        (argTypes, returnType) <- lookupFun env id
        passedTypes <- mapRight (inferExp env) args
        if argTypes == passedTypes
          then
            Ok returnType
          else
            Bad ("wrong type of function. Expected " ++ show argTypes ++ " but got " ++ show passedTypes)

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type 
inferBin types env exp1 exp2 = do
    typ <- inferExp env exp1 
    if elem typ types
      then
        checkExp env exp2 typ
      else
        Bad ("wrong type of expression " ++ printTree exp1)

checkExp :: Env -> Exp -> Type -> Err Type
checkExp env exp typ = do
  t <- inferExp env exp 
  if t == typ
    then
      Ok typ
    else 
      Bad ("wrong type of expression " ++ printTree exp ++" actual type: " ++ show t ++ "expected type: " ++ show typ)

-- Help functions

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
