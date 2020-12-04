{-# LANGUAGE LambdaCase #-}

-- | Typed syntax for C--.

module Annotated where

import qualified CMM.Abs as A
import qualified Prelude as C (Eq, Ord, Show, Read)
import Prelude (Double, Integer)
-- This is a stub for typed ASTs produced by the type checker
-- as input for the compiler.

-- To make the stub compile, we just define an alias to
-- untyped ASTs here.

type Program = A.Program
type Exp = (AExp, A.Type)

data Stm
    = SExp AExp
    | SDecls A.Type [A.Id]
    | SInit A.Type A.Id Exp
    | SReturn Exp
    | SWhile Exp Stm
    | SBlock [Stm]
    | SIfElse Exp Stm Stm
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data AExp
    = EBool A.BoolLit
    | EInt Integer
    | EDouble Double
    | EId A.Id
    | EApp A.Id [Exp]
    | EPost A.Id A.IncDecOp
    | EPre A.IncDecOp A.Id
    | EMul Exp A.MulOp Exp
    | EAdd Exp A.AddOp Exp
    | ECmp Exp A.CmpOp Exp
    | EAnd Exp Exp
    | EOr Exp Exp
    | EAss A.Id Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)