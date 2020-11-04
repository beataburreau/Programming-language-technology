-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParLab where
import qualified AbsLab
import LexLab
}

%name pProgram Program
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  ')' { PT _ (TS _ 2) }
  ',' { PT _ (TS _ 3) }
  '::' { PT _ (TS _ 4) }
  ';' { PT _ (TS _ 5) }
  '<<' { PT _ (TS _ 6) }
  '=' { PT _ (TS _ 7) }
  '>>' { PT _ (TS _ 8) }
  'bool' { PT _ (TS _ 9) }
  'char' { PT _ (TS _ 10) }
  'const' { PT _ (TS _ 11) }
  'double' { PT _ (TS _ 12) }
  'int' { PT _ (TS _ 13) }
  'return' { PT _ (TS _ 14) }
  'void' { PT _ (TS _ 15) }
  '{' { PT _ (TS _ 16) }
  '}' { PT _ (TS _ 17) }
  L_Ident  { PT _ (TV $$) }
  L_integ  { PT _ (TI $$) }
  L_doubl  { PT _ (TD $$) }
  L_charac { PT _ (TC $$) }
  L_quoted { PT _ (TL $$) }

%%

Ident :: { AbsLab.Ident}
Ident  : L_Ident { AbsLab.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read ($1)) :: Integer }

Double  :: { Double }
Double   : L_doubl  { (read ($1)) :: Double }

Char    :: { Char }
Char     : L_charac { (read ($1)) :: Char }

String  :: { String }
String   : L_quoted { $1 }

Program :: { AbsLab.Program }
Program : ListDef { AbsLab.PDefs $1 }

ListDef :: { [AbsLab.Def] }
ListDef : {- empty -} { [] } | Def ListDef { (:) $1 $2 }

Def :: { AbsLab.Def }
Def : Type Ident '(' ListArg ')' Body { AbsLab.DFunc $1 $2 $4 $6 }
    | Const { AbsLab.DConst $1 }

ListArg :: { [AbsLab.Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }

Body :: { AbsLab.Body }
Body : '{' ListStm '}' { AbsLab.FBody $2 } | ';' { AbsLab.EBody }

ListStm :: { [AbsLab.Stm] }
ListStm : {- empty -} { [] } | Stm ListStm { (:) $1 $2 }

Type :: { AbsLab.Type }
Type : 'int' { AbsLab.TInt }
     | 'bool' { AbsLab.TBool }
     | 'char' { AbsLab.TChar }
     | 'double' { AbsLab.TDouble }
     | 'void' { AbsLab.TVoid }
     | Const { AbsLab.TConst $1 }

Arg :: { AbsLab.Arg }
Arg : Spec Type AId { AbsLab.ADecl $1 $2 $3 }

Spec :: { AbsLab.Spec }
Spec : 'const' { AbsLab.CSpec } | {- empty -} { AbsLab.ESpec }

AId :: { AbsLab.AId }
AId : Id { AbsLab.SId $1 } | {- empty -} { AbsLab.EId }

Id :: { AbsLab.Id }
Id : Ident { AbsLab.VDecl $1 }
   | Ident '=' Exp { AbsLab.VInit $1 $3 }

Stm :: { AbsLab.Stm }
Stm : Exp ';' { AbsLab.SExp $1 }
    | Spec Type ListId ';' { AbsLab.SDecl $1 $2 $3 }
    | 'return' Exp ';' { AbsLab.SRet $2 }
    | '{' ListStm '}' { AbsLab.SBlock $2 }

ListId :: { [AbsLab.Id] }
ListId : Id { (:[]) $1 } | Id ',' ListId { (:) $1 $3 }

Exp15 :: { AbsLab.Exp }
Exp15 : Integer { AbsLab.EInt $1 }
      | Double { AbsLab.EDouble $1 }
      | Char { AbsLab.EChar $1 }
      | String { AbsLab.EString $1 }
      | '(' Exp ')' { $2 }
      | Const { AbsLab.EConst $1 }

Exp10 :: { AbsLab.Exp }
Exp10 : Exp8 '<<' Exp9 { AbsLab.ELShift $1 $3 }
      | Exp8 '>>' Exp9 { AbsLab.ERShift $1 $3 }
      | Exp11 { $1 }

Exp :: { AbsLab.Exp }
Exp : Exp1 { $1 }

Exp1 :: { AbsLab.Exp }
Exp1 : Exp2 { $1 }

Exp2 :: { AbsLab.Exp }
Exp2 : Exp3 { $1 }

Exp3 :: { AbsLab.Exp }
Exp3 : Exp4 { $1 }

Exp4 :: { AbsLab.Exp }
Exp4 : Exp5 { $1 }

Exp5 :: { AbsLab.Exp }
Exp5 : Exp6 { $1 }

Exp6 :: { AbsLab.Exp }
Exp6 : Exp7 { $1 }

Exp7 :: { AbsLab.Exp }
Exp7 : Exp8 { $1 }

Exp8 :: { AbsLab.Exp }
Exp8 : Exp9 { $1 }

Exp9 :: { AbsLab.Exp }
Exp9 : Exp10 { $1 }

Exp11 :: { AbsLab.Exp }
Exp11 : Exp12 { $1 }

Exp12 :: { AbsLab.Exp }
Exp12 : Exp13 { $1 }

Exp13 :: { AbsLab.Exp }
Exp13 : Exp14 { $1 }

Exp14 :: { AbsLab.Exp }
Exp14 : Exp15 { $1 }

Const :: { AbsLab.Const }
Const : ListIdent { AbsLab.QConst $1 }

ListIdent :: { [AbsLab.Ident] }
ListIdent : {- empty -} { [] }
          | Ident { (:[]) $1 }
          | Ident '::' ListIdent { (:) $1 $3 }
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

