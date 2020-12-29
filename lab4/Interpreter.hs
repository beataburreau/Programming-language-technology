{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Fun.Abs
import Fun.Print

type Err = Except String

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Entry point: Program computes a number.

interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do

  -- Create initial environment
  -- mkLam [x1,...,xn] e = EAbs x1 $ ... $ EAbs xn e
  -- mkLam [x1,...,xn] e = x1 `EAbs` ... xn `EAbs` e
  -- EAbs :: Ident -> Exp -> Exp
  let mkLam xs e = foldr EAbs e xs
  let mkDef (DDef f xs e) = (f, mkLam xs e)
  let sig = Map.fromList $ map mkDef defs
  let cxt = Cxt
        { cxtStrategy = strategy
        , cxtSig      = sig
        , cxtEnv      = Map.empty
        }

  -- Run the interpreter.
  v <- eval cxt mainExp

  -- Return the result.
  intValue v

todo s = error $ unwords [ s, "not yet implemented, TODO!"]

---------------------------------------------------------------------------
-- * Data structures for the interpreter.
---------------------------------------------------------------------------

-- | Context.

data Cxt = Cxt
  { cxtStrategy :: Strategy  -- ^ Evaluation strategy (fixed).
  , cxtSig      :: Sig       -- ^ Binds function identifiers to expression.
  , cxtEnv      :: Env       -- ^ Binds local variables to values.
  }

-- | Values.
data Value
  = VInt Integer             -- ^ Integer literal.
  | VClos Ident Exp Env      -- ^ Function: lambda-closure ⟨λx→e;γ⟩
                             --   FV(e) ⊆ {x} ∪ dom(γ)

instance Show Value where
  show (VInt a) = "VInt " ++ show a
  show (VClos i e env) = "VClos (" ++ show i ++ ") (" ++ show e ++ ") (" ++ show env ++ ")"


-- fromList [(Ident "x",VClos (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))) (fromList []))] 
-- fromList [(Ident "double",EAbs (Ident "x") (EAdd (EVar (Ident "x")) (EVar (Ident "x")))),(Ident "twice1",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice2",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice3",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice4",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice5",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice6",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice7",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))))),(Ident "twice8",EAbs (Ident "f") (EAbs (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x"))))))] 
instance Num Value where
  (VInt a) + (VInt b) = VInt (a+b)
  (VInt a) - (VInt b) = VInt (a-b)
  fromInteger i = VInt i
instance Eq Value where
  (VInt a) == (VInt b) = a == b
instance Ord Value where
  compare (VInt a) (VInt b) = compare a b

-- | Signature.
type Sig = Map Ident Exp

-- | Environment.
type Env = Map Ident Entry
type Entry = Value

intValue :: Value -> Err Integer
intValue = \case
  VInt i  -> return i
  VClos{} -> throwError "Integer value expected, but got function value"

apply :: Cxt -> Value -> Value -> Err Value
apply cxt f v =
  case f of
    VInt{} -> throwError "Integer value cannot be applied, expected function value"
    VClos x e env -> eval cxt{ cxtEnv = Map.insert x v env } e

---------------------------------------------------------------------------
-- * Interpreter.
---------------------------------------------------------------------------

-- | Evaluation.
eval :: Cxt -> Exp -> Err Value
eval cxt = \case

  EVar x    -> do
    case Map.lookup x $ cxtEnv cxt of
      Just v | CallByValue <- cxtStrategy cxt -> return v
      Just (VClos i e env) -> eval cxt{ cxtEnv = env } e
      Just v -> return v
      Nothing -> case Map.lookup x $ cxtSig cxt of
        Just e -> eval cxt{ cxtEnv = Map.empty } e
        Nothing -> throwError $ unwords [ "unbound variable", printTree x, show $ cxtEnv cxt, show $ cxtSig cxt ]
-- [(Ident "x", VClos (Ident "x") (EApp (EVar (Ident "f")) (EApp (EVar (Ident "f")) (EVar (Ident "x")))) (fromList []))]
  EInt i    -> return $ VInt i

  EApp f a | CallByName <- cxtStrategy cxt -> do
    g <- eval cxt f
    let v = VClos (closVar g) a $ cxtEnv cxt
    apply cxt g v

  EApp f a | CallByValue <- cxtStrategy cxt -> do
    g <- eval cxt f
    v <- eval cxt a
    apply cxt g v

  EAdd e e' -> do
    v <- evalInt cxt e
    v' <- evalInt cxt e'
    return (v + v')

  ESub e e' -> do
    v <- evalInt cxt e
    v' <- evalInt cxt e'
    return (v - v')

  ELt e e' -> do
    v <- evalInt cxt e
    v' <- evalInt cxt e'
    if v < v'
      then
        return 1
      else
        return 0

  EIf c t e -> do
    c' <- evalInt cxt c
    if c' == 1
      then
        eval cxt t
      else
        eval cxt e

  EAbs x e  -> return $ VClos x e (cxtEnv cxt)

evalInt :: Cxt -> Exp -> Err Value
evalInt cxt e = do
  i <- eval cxt e
  case i of
    (VInt i) -> return (VInt i)
    _ -> throwError "Expected integer"

closVar :: Value -> Ident
closVar (VClos i _ _) = i
