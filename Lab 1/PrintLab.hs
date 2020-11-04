{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintLab.
--   Generated by the BNF converter.

module PrintLab where

import qualified AbsLab
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsLab.Ident where
  prt _ (AbsLab.Ident i) = doc $ showString $ i
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "::"), prt 0 xs]

instance Print AbsLab.Program where
  prt i e = case e of
    AbsLab.PDefs defs -> prPrec i 0 (concatD [prt 0 defs])

instance Print [AbsLab.Def] where
  prt = prtList

instance Print AbsLab.Def where
  prt i e = case e of
    AbsLab.DFunc type_ id args body -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 body])
    AbsLab.DConst const -> prPrec i 0 (concatD [prt 0 const])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsLab.Arg] where
  prt = prtList

instance Print AbsLab.Body where
  prt i e = case e of
    AbsLab.FBody stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsLab.EBody -> prPrec i 0 (concatD [doc (showString ";")])

instance Print [AbsLab.Stm] where
  prt = prtList

instance Print AbsLab.Type where
  prt i e = case e of
    AbsLab.TInt -> prPrec i 0 (concatD [doc (showString "int")])
    AbsLab.TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsLab.TChar -> prPrec i 0 (concatD [doc (showString "char")])
    AbsLab.TDouble -> prPrec i 0 (concatD [doc (showString "double")])
    AbsLab.TVoid -> prPrec i 0 (concatD [doc (showString "void")])
    AbsLab.TConst const -> prPrec i 0 (concatD [prt 0 const])

instance Print AbsLab.Arg where
  prt i e = case e of
    AbsLab.ADecl spec type_ aid -> prPrec i 0 (concatD [prt 0 spec, prt 0 type_, prt 0 aid])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsLab.Spec where
  prt i e = case e of
    AbsLab.CSpec -> prPrec i 0 (concatD [doc (showString "const")])
    AbsLab.ESpec -> prPrec i 0 (concatD [])

instance Print AbsLab.AId where
  prt i e = case e of
    AbsLab.SId id -> prPrec i 0 (concatD [prt 0 id])
    AbsLab.EId -> prPrec i 0 (concatD [])

instance Print AbsLab.Id where
  prt i e = case e of
    AbsLab.VDecl id -> prPrec i 0 (concatD [prt 0 id])
    AbsLab.VInit id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsLab.Stm where
  prt i e = case e of
    AbsLab.SExp exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString ";")])
    AbsLab.SDecl spec type_ ids -> prPrec i 0 (concatD [prt 0 spec, prt 0 type_, prt 0 ids, doc (showString ";")])
    AbsLab.SRet exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    AbsLab.SBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [AbsLab.Id] where
  prt = prtList

instance Print AbsLab.Exp where
  prt i e = case e of
    AbsLab.EInt n -> prPrec i 15 (concatD [prt 0 n])
    AbsLab.EDouble d -> prPrec i 15 (concatD [prt 0 d])
    AbsLab.EChar c -> prPrec i 15 (concatD [prt 0 c])
    AbsLab.EString str -> prPrec i 15 (concatD [prt 0 str])
    AbsLab.ELShift exp1 exp2 -> prPrec i 10 (concatD [prt 8 exp1, doc (showString "<<"), prt 9 exp2])
    AbsLab.ERShift exp1 exp2 -> prPrec i 10 (concatD [prt 8 exp1, doc (showString ">>"), prt 9 exp2])
    AbsLab.EConst const -> prPrec i 15 (concatD [prt 0 const])

instance Print AbsLab.Const where
  prt i e = case e of
    AbsLab.QConst ids -> prPrec i 0 (concatD [prt 0 ids])

instance Print [AbsLab.Ident] where
  prt = prtList

