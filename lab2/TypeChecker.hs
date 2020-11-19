module TypeChecker where

import Control.Monad

import Data.Map (Map)
import Data.Either
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

type Env = [[(String,Type)]]

-- check a whole program
typecheck :: Program -> Err ()
typecheck p = return ()

lookupVar :: Env -> Id -> Err Type
lookupVar = undefined

lookupFun :: Env -> Id -> Err ([Type],Type)
lookupFun = undefined

updateVar :: Env -> Id -> Type -> Err Env
updateVar (block:env) (Id id) typ = do
  if elem id $ map fst block
    then
      return Bad "Variable " ++ id ++ " already defined in block."
    else
      return Ok env

-- updateFun :: Env -> Id -> ([Type],Type) -> Err Env


newBlock :: Env -> Env
newBlock env = []:env

emptyEnv  :: Env
emptyEnv = []

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
            return returnType
          else     
            Bad ("wrong type of function. Expected " ++ argTypes ++ " but got " ++ passedTypes)

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type 
inferBin types env exp1 exp2 = do
    typ <- inferExp env exp1 
    if elem typ types
      then
        checkExp env exp2 typ
      else
        Bad ("wrong type of expression " ++ printTree exp1)

checkExp :: Env -> Exp -> Type -> Err Type
checkExp = undefined