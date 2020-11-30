module Interpreter where

import Control.Monad

import System.IO.Error

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

data Val = VBool BoolLit
    | VInt Integer
    | VDouble Double
    deriving (Prelude.Eq, Prelude.Ord, Prelude.Read)

instance Show Val where
    show (VBool a) = show a
    show (VInt a) = show a
    show (VDouble a) = show a

type Env = ([Def], [[(Id, Maybe Val)]])

interpret :: Program -> IO ()
interpret (PDefs p) = do
    let env = (predefinedFunctions ++ p,[[]])
    let (DFun _ _ _ stms) = lookupFun (Id "main") env
    execs stms env
    return ()

predefinedFunctions :: [Def]
predefinedFunctions = [DFun Type_void (Id "printInt") [ADecl Type_int (Id "x")] [], DFun Type_void (Id "printDouble") [ADecl Type_double (Id "x")] [], 
                        DFun Type_int (Id "readInt") [] [], DFun Type_double (Id "readDouble") [] []]

evalArgs :: ([(Id, Val)], Env) -> [Arg] -> [Exp] -> IO ([(Id, Maybe Val)], Env)
evalArgs (vals, env) [] [] = return (map (\(id, val) -> (id, Just val)) vals, env)
evalArgs (vals, env) ((ADecl typ id):as) (e:es) = do 
    (v, env2) <- eval e env 
    if typ == Type_double
        then 
            evalArgs ((id, toDouble v):vals, env2) as es
        else    
            evalArgs ((id, v):vals, env2) as es

eval  :: Exp   -> Env -> IO (Val, Env)
eval (EBool b) env = return (VBool b, env)
eval (EInt i) env = return (VInt i, env)
eval (EDouble g) env = return (VDouble g, env)
eval (EId id) env = case lookupVal id env of
    Just v -> return (v, env)
    Nothing -> ioError (userError ("Variable " ++ show id ++ " not assigned"))
eval (EApp (Id id) [n]) env | id == "printInt" = do
    (str, env1) <- eval n env
    print str
    return (VBool LTrue, env1)
eval (EApp (Id id) [n]) env | id == "printDouble" = do
    (str, env1) <- eval n env
    print (toDouble str)
    return (VBool LTrue, env1)    
eval (EApp (Id "readInt") []) env = do
    input <- getLine
    return (VInt (read input), env)
eval (EApp (Id "readDouble") []) env = do
    input <- getLine
    return (VDouble (read input), env)
eval (EApp id exps) (defs, vals) = do
    (newVals, env) <- evalArgs ([], (defs, vals)) args exps
    r <- execs stms (defs, newVals:vals)
    case r of
        Left val -> if typ == Type_double then return (toDouble val, env) else return (val, env)
        Right _ -> return (VBool LTrue, env)
    where DFun typ _ args stms = lookupFun id (defs, vals)
eval (EPost id op) (defs, vals) = case lookupVal id (defs, vals) of
    Just val -> return (val, (defs, updateVal vals id (incDecVal val op)))
eval (EPre op id) (defs, vals) = case lookupVal id (defs, vals) of
    Just val -> return (incDecVal val op, (defs, updateVal vals id (incDecVal val op)))
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
    return (toBoolLit (compareVal v1 op v2), e2)
eval (EAnd exp1 exp2) env = do
    (v1, e1) <- eval exp1 env
    if v1 == VBool LFalse
        then
            return (VBool LFalse, e1)
        else do
            (v2, e2) <- eval exp2 e1
            return (vAnd v1 v2, e2)
eval (EOr exp1 exp2) env = do
    (v1, e1) <- eval exp1 env
    if v1 == VBool LTrue
        then 
            return (VBool LTrue, e1)
        else do
            (v2, e2) <- eval exp2 e1
            return (vOr v1 v2, e2)
eval (EAss id exp) env = do
    (v1, (defs, vals)) <- eval exp env 
    if lookupType id env == Type_double
        then
            return (toDouble v1, (defs, updateVal vals id (toDouble v1)))
        else
            return (v1, (defs, updateVal vals id v1))

compareVal :: Val -> CmpOp -> Val -> Bool
compareVal (VBool v1) op (VBool v2) = compOp op (VBool v1) (VBool v2)
compareVal v1 op v2 = compOp op (toDouble v1) (toDouble v2)

toDouble :: Val -> Val
toDouble (VInt a) = VDouble (fromIntegral a)
toDouble (VDouble a) = VDouble a

compOp :: CmpOp -> Val -> Val -> Bool
compOp OLt a b = a < b
compOp OGt a b = a > b
compOp OLtEq a b = a <= b
compOp OGtEq a b = a >= b
compOp OEq a b = a == b
compOp ONEq a b = a /= b

updateVal :: [[(Id, Maybe Val)]] -> Id -> Val -> [[(Id, Maybe Val)]]
updateVal env1 id val = env2
    where (_:env2) = updteVal [[]] env1 id (Just val)

updteVal :: [[(Id, Maybe Val)]] -> [[(Id, Maybe Val)]] -> Id -> Maybe Val -> [[(Id, Maybe Val)]]
updteVal _ [] _ _ = undefined
updteVal head (t:tail) id val | elem id (map fst t) = head ++ [map (\(i,v) -> if i == id then (i,val) else (i,v)) t] ++ tail 
updteVal head (t:tail) id val = updteVal (head ++ [t]) tail id val

-- [] [4] [3] [2] [1]
-- [4] 3 [2] [1]
-- [3, 4]
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
divide (VDouble a) (VInt b) = VDouble (a / d)
    where (VDouble d) = toDouble (VInt b)
divide (VInt a) (VDouble b) = VDouble (d / b)
    where (VDouble d) = toDouble (VInt a)

plus :: Val -> Val -> Val
plus (VDouble a) (VDouble b) = VDouble (a + b)
plus (VInt a ) (VInt b) = VInt (a + b)
plus (VDouble a) (VInt b) = VDouble (a + d)
    where (VDouble d) = toDouble (VInt b)
plus (VInt a) (VDouble b) = VDouble (d + b)
    where (VDouble d) = toDouble (VInt a)

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
    where vars = zip ids (repeat Nothing)
exec (SInit typ id exp) (defs, vals) = do
    (v, (defs1, block:vals1)) <- eval exp (defs, vals)
    if typ == Type_double
        then 
            return (Right (defs1, ((id, Just (toDouble v)):block):vals1))
        else
            return (Right (defs1, ((id, Just v):block):vals1))
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
        Right (defs, _:newVals) -> return (Right (defs, newVals))
exec (SIfElse exp stm1 stm2) env = do
    (v, env1) <- eval exp env
    if isTrue v
        then
            execs [SBlock [stm1]] env1
        else
            execs [SBlock [stm2]] env1

isTrue :: Val -> Bool
isTrue a = not (isFalse a)

isFalse :: Val -> Bool
isFalse (VBool LFalse) = True
isFalse (VInt 0) = True
isFalse (VDouble 0.0) = True
isFalse _ = False

execs :: [Stm] -> Env -> IO (Either Val Env)
execs [] env = do return (Right env) 
execs (s:ss) (defs, vals) = do
    r <- exec s (defs, vals)
    case r of
        Left  v    -> return (Left v)
        Right (_,vals1) -> execs ss (defs, vals1)

lookupType :: Id -> Env -> Type
lookupType id (defs, block:vals) = case lookup id block of
    Just (Just (VDouble _)) -> Type_double
    Just (Just (VInt _)) -> Type_int
    Just (Just (VBool _)) -> Type_bool
    Just Nothing -> Type_void
    Nothing -> lookupType id (defs, vals) 

lookupVal :: Id -> Env -> Maybe Val
lookupVal id (defs, block:vals) = case lookup id block of
    Just v -> v
    Nothing -> lookupVal id (defs, vals)

lookupFun :: Id -> Env -> Def
lookupFun id ((DFun typ fId args stms):_, _) | id == fId = DFun typ fId args stms
lookupFun id (_:defs, vals) = lookupFun id (defs, vals)
