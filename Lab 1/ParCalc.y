-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParCalc where
import qualified AbsCalc
import LexCalc
}

%name pProgram Program
%name pExp1 Exp1
%name pExp2 Exp2
%name pProgram1 Program1
%name pProgram2 Program2
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  '*' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  '-' { PT _ (TS _ 5) }
  '/' { PT _ (TS _ 6) }
  L_integ  { PT _ (TI $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read ($1)) :: Integer }

Program :: { AbsCalc.Program }
Program : Program '+' Exp1 { AbsCalc.EAdd $1 $3 }
        | Program '-' Exp1 { AbsCalc.ESub $1 $3 }
        | Program1 { $1 }

Exp1 :: { AbsCalc.Exp }
Exp1 : Exp1 '*' Exp2 { AbsCalc.EMul $1 $3 }
     | Exp1 '/' Exp2 { AbsCalc.EDiv $1 $3 }

Exp2 :: { AbsCalc.Exp }
Exp2 : Integer { AbsCalc.EInt $1 }

Program1 :: { AbsCalc.Program }
Program1 : Program2 { $1 }

Program2 :: { AbsCalc.Program }
Program2 : '(' Program ')' { $2 }
{

happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
}

