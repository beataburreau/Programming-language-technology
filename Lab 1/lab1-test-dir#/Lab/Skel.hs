-- Haskell module generated by the BNF converter

module Lab.Skel where

import qualified Lab.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Lab.Abs.Ident -> Result
transIdent x = case x of
  Lab.Abs.Ident string -> failure x
transProgram :: Lab.Abs.Program -> Result
transProgram x = case x of
  Lab.Abs.PDefs defs -> failure x
transDef :: Lab.Abs.Def -> Result
transDef x = case x of
  Lab.Abs.DFunc type_ ident args body -> failure x
  Lab.Abs.DConst const -> failure x
transBody :: Lab.Abs.Body -> Result
transBody x = case x of
  Lab.Abs.FBody stms -> failure x
  Lab.Abs.EBody -> failure x
transType :: Lab.Abs.Type -> Result
transType x = case x of
  Lab.Abs.TInt -> failure x
  Lab.Abs.TBool -> failure x
  Lab.Abs.TChar -> failure x
  Lab.Abs.TDouble -> failure x
  Lab.Abs.TVoid -> failure x
  Lab.Abs.TConst const -> failure x
transArg :: Lab.Abs.Arg -> Result
transArg x = case x of
  Lab.Abs.ADecl spec type_ aid -> failure x
transSpec :: Lab.Abs.Spec -> Result
transSpec x = case x of
  Lab.Abs.CSpec -> failure x
  Lab.Abs.ESpec -> failure x
transAId :: Lab.Abs.AId -> Result
transAId x = case x of
  Lab.Abs.SId id -> failure x
  Lab.Abs.EId -> failure x
transId :: Lab.Abs.Id -> Result
transId x = case x of
  Lab.Abs.VDecl ident -> failure x
  Lab.Abs.VInit ident exp -> failure x
transStm :: Lab.Abs.Stm -> Result
transStm x = case x of
  Lab.Abs.SExp exp -> failure x
  Lab.Abs.SDecl spec type_ ids -> failure x
  Lab.Abs.SRet exp -> failure x
  Lab.Abs.SBlock stms -> failure x
transExp :: Lab.Abs.Exp -> Result
transExp x = case x of
  Lab.Abs.EInt integer -> failure x
  Lab.Abs.EDouble double -> failure x
  Lab.Abs.EChar char -> failure x
  Lab.Abs.EString string -> failure x
  Lab.Abs.ELShift exp1 exp2 -> failure x
  Lab.Abs.ERShift exp1 exp2 -> failure x
  Lab.Abs.EConst const -> failure x
transConst :: Lab.Abs.Const -> Result
transConst x = case x of
  Lab.Abs.QConst idents -> failure x

