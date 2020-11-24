module Interpreter where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

import TypeChecker

data Val 
    = VBool BoolLit
    | VInt Integer
    | VDouble Double

interpret :: Program -> IO ()
interpret p = lookupFun (Id "main") p

eval  :: Exp   -> Sig -> Env -> IO (Val, Env)
eval exp sig env = undefined

exec  :: Stm   -> Sig -> Env -> IO (Either Val Env)
exec stm sig env = undefined

execs :: [Stm] -> Sig -> Env -> IO (Either Val Env)
execs stms sig env = undefined

-- Val lookup (Ident x, Env γ)
lookupFun :: Id -> Env -> Def
lookupFun id env = undefined
-- Env extend (Env γ, Ident x, Val v)
-- Env extend (Env γ, Def d)
-- Env newBlock (Env γ)
-- Env exitBlock (Env γ)
-- Env emptyEnv ()
