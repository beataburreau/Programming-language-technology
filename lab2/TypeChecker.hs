module TypeChecker where

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map

import CMM.Abs
import CMM.Print
import CMM.ErrM

type Env = [[(String,Type)]]

-- check a whole program
typecheck :: Program -> Err ()
typecheck p = return ()

-- check function definition
-- typecheck :: Env -> Def -> ()