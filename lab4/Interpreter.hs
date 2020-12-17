{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad       (MonadPlus(..), liftM)

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Fun.Abs
import Fun.Print

--type Err = Except String

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Monad Err where
  return      = Ok
  Ok a  >>= f = f a
  Bad s >>= _ = Bad s

instance Applicative Err where
  pure = Ok
  (Bad s) <*> _ = Bad s
  (Ok f) <*> o  = liftM f o

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x

instance Alternative Err where
  empty = mzero
  (<|>) = mplus

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

-- | Signature.
type Sig = Map Ident Exp

-- | Environment.
type Env = Map Ident Entry
type Entry = Value

intValue :: Value -> Err Integer
intValue = \case
  VInt i  -> Ok i
  VClos{} -> Bad "Integer value expected, but got function value"

apply :: Cxt -> Value -> Value -> Err Value
apply cxt f v =
  case f of
    VInt{} -> Bad "Integer value cannot be applied, expected function value"
    VClos x e env -> eval cxt{ cxtEnv = Map.insert x v env } e

---------------------------------------------------------------------------
-- * Interpreter.
---------------------------------------------------------------------------

-- | Evaluation.
eval :: Cxt -> Exp -> Err Value
eval cxt = \case

  EInt i    -> Ok $ VInt i

  EVar x    -> do
    case Map.lookup x $ cxtEnv cxt of
      Just v  -> Ok v
      Nothing -> case Map.lookup x $ cxtSig cxt of
        Just e  -> eval cxt{ cxtEnv = Map.empty } e
        Nothing -> Bad $ unwords [ "unbound variable", printTree x ]


  EAbs x e  -> Ok $ VClos x e (cxtEnv cxt)

  EApp f a | CallByValue <- cxtStrategy cxt -> do
    g <- eval cxt f
    v <- eval cxt a
    apply cxt g v

  EAdd e e' -> todo "EAdd"

  ESub e e' -> todo "ESub"

  ELt  e e' -> todo "ELt"

  EIf c t e -> todo "EIf"
