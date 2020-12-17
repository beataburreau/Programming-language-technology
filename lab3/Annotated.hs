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

data Program = PDefs [Def]
  deriving (C.Eq, C.Ord, C.Show, C.Read)
type Exp = (AExp, A.Type)

data Def = DFun A.Type A.Id [A.Arg] [Stm]
  deriving (C.Eq, C.Ord, C.Show, C.Read)

data Stm
    = SExp Exp A.Type
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