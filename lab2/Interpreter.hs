module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

type Env = ([Def], [[(Id, Val)]])

interpret :: Program -> IO ()
interpret (PDefs p) = do
    let env = (predefinedFunctions ++ p,[[]])
    let (DFun _ _ _ stms) = lookupFun (Id "main") env
    execs stms env
    return ()

predefinedFunctions :: [Def]
predefinedFunctions = [DFun Type_void (Id "printInt") [ADecl Type_int (Id "x")] [], DFun Type_void (Id "printDouble") [ADecl Type_double (Id "x")] [], 
                        DFun Type_int (Id "readInt") [] [], DFun Type_double (Id "readDouble") [] []]

eval  :: Exp   -> Env -> IO (Val, Env)
eval (EBool b) env = return (VBool b, env)
eval (EInt i) env = return (VInt i, env)
eval (EDouble g) env = return (VDouble g, env)
eval (EId id) env = return (lookupVal id env, env)
eval (EApp id exps) env = undefined
eval (EPost id op) (defs, block:vals) = return (val, (defs, ((id, incDecVal val op):block):vals))
    where val = lookupVal id (defs, block:vals)
eval (EPre op id) (defs, block:vals) = return (val, (defs, ((id, val):block):vals))
    where val = incDecVal (lookupVal id (defs, block:vals)) op
eval (EMul exp1 op exp2) env = do
    (v1, e1) <- eval exp1 env
    (v2, e2) <- eval exp2 e1
    case op of
        OTimes -> return (times v1 v2, e2)
        ODiv -> return (divide v1 v2, e2)
eval (EAdd exp1 op exp2) env = do
    (v1, e1) <- eval exp1 env
    (v2, e2) <- eval exp2 e1
    case op of
        OPlus -> return (plus v1 v2, e2)
        OMinus -> return (minus v1 v2, e2)
eval (ECmp exp1 op exp2) env = do
    (v1, e1) <- eval exp1 env
    (v2, e2) <- eval exp2 e1
    case op of
        OLt -> return (toBoolLit (v1 < v2), e2)
        OGt -> return (toBoolLit (v1 > v2), e2)
        OLtEq -> return (toBoolLit (v1 <= v2), e2)
        OGtEq -> return (toBoolLit (v1 >= v2), e2)
        OEq -> return (toBoolLit (v1 == v2), e2)
        ONEq -> return (toBoolLit (v1 /= v2), e2)
eval (EAnd exp1 exp2) env = do
    (v1, e1) <- eval exp1 env
    (v2, e2) <- eval exp2 e1
    return (vAnd v1 v2, e2)
eval (EOr exp1 exp2) env = do
    (v1, e1) <- eval exp1 env
    (v2, e2) <- eval exp2 e1
    return (vOr v1 v2, e2)
eval (EAss id exp) env = do
    (v1, (defs, block:vals)) <- eval exp env 
    return (v1, (defs, ((id, v1):block):vals))

toBoolLit :: Bool -> Val
toBoolLit True = VBool LTrue
toBoolLit False = VBool LFalse

incDecVal :: Val -> IncDecOp -> Val
incDecVal (VInt i) OInc = VInt (i+1)
incDecVal (VInt i) ODec = VInt (i-1)
incDecVal (VDouble d) OInc = VDouble (d+1)
incDecVal (VDouble d) ODec = VDouble (d-1)

times :: Val -> Val -> Val
times (VDouble a) (VDouble b) = VDouble (a * b)
times (VInt a) (VInt b) = VInt (a * b)

divide :: Val -> Val -> Val
divide (VDouble a) (VDouble b) = VDouble (a / b)
divide (VInt a ) (VInt b) = VInt (div a b)

plus :: Val -> Val -> Val
plus (VDouble a) (VDouble b) = VDouble (a + b)
plus (VInt a ) (VInt b) = VInt (a + b)

minus :: Val -> Val -> Val
minus (VDouble a) (VDouble b) = VDouble (a - b)
minus (VInt a ) (VInt b) = VInt (a - b)

vAnd :: Val -> Val -> Val 
vAnd (VBool LTrue) (VBool LTrue) = VBool LTrue
vAnd _ _ = VBool LFalse

vOr :: Val -> Val -> Val 
vOr (VBool LFalse) (VBool LFalse) = VBool LFalse
vOr _ _ = VBool LTrue

exec  :: Stm   -> Env -> IO (Either Val Env)
exec (SExp exp) env = do
    (_, e) <- eval exp env
    return (Right e)
exec (SDecls typ ids) (defs, block:vals) = do
    return (Right (defs, (vars ++ block):vals))
    where vars = zip ids (repeat (initVal typ))
exec (SInit _ id exp) (defs, vals) = do
    (v, (defs1, block:vals1)) <- eval exp (defs, vals)
    return (Right (defs1, ((id, v):block):vals1))
exec (SReturn exp) env = do
    (v, _) <- eval exp env
    return (Left v)
exec (SWhile exp stm) env = do
    (v, env1) <- eval exp env 
    if isFalse v then return (Right env1)
    else execs [SBlock [stm], SWhile exp stm] env1
exec (SBlock stms) (defs, vals) = do
    r <- execs stms (defs, []:vals)
    case r of
        Left v -> return (Left v)
        Right (defs, _:vals) -> return (Right (defs, vals))
exec (SIfElse exp stm1 stm2) env = do
    (v, env1) <- eval exp env
    if isTrue v
        then
            execs [SBlock [stm1]] env1
        else
            execs [SBlock [stm2]] env1

initVal :: Type -> Val
initVal Type_bool = VBool LFalse
initVal Type_int = VInt 0
initVal Type_double = VDouble 0.0

isTrue :: Val -> Bool
isTrue a = not (isFalse a)

isFalse :: Val -> Bool
isFalse (VBool LFalse) = True
isFalse (VInt 0) = True
isFalse (VDouble 0.0) = True
isFalse _ = False

execs :: [Stm] -> Env -> IO (Either Val Env)
execs (s:ss) (defs, vals) = do
    r <- exec s (defs, vals)
    case r of
        Left  v    -> return (Left v)
        Right (_,vals1) -> execs ss (defs, vals1)

lookupVal :: Id -> Env -> Val
lookupVal id (defs, block:vals) = case lookup id block of
    Just v -> v
    Nothing -> lookupVal id (defs, vals)

lookupFun :: Id -> Env -> Def
lookupFun id ((DFun typ fId args stms):_, _) | id == fId = DFun typ fId args stms
lookupFun id (_:defs, vals) = lookupFun id (defs, vals)
-- Env extend (Env γ, Ident x, Val v)
-- Env extend (Env γ, Def d)
-- Env newBlock (Env γ)
-- Env exitBlock (Env γ)
-- Env emptyEnv ()
