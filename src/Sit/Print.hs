{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Sit.
--   Generated by the BNF converter.

module Sit.Print where

import qualified Sit.Abs
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

instance Print Sit.Abs.Ident where
  prt _ (Sit.Abs.Ident i) = doc $ showString $ i
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Sit.Abs.Prg where
  prt i e = case e of
    Sit.Abs.Prg decls -> prPrec i 0 (concatD [prt 0 decls])

instance Print Sit.Abs.Decl where
  prt i e = case e of
    Sit.Abs.Sig id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString ":"), prt 0 exp])
    Sit.Abs.Def id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp])
    Sit.Abs.Open qualid -> prPrec i 0 (concatD [doc (showString "open"), doc (showString "import"), prt 0 qualid])
    Sit.Abs.Blank -> prPrec i 0 (concatD [])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString "--;"), prt 0 xs]

instance Print Sit.Abs.QualId where
  prt i e = case e of
    Sit.Abs.Sg id -> prPrec i 0 (concatD [prt 0 id])
    Sit.Abs.Cons qualid id -> prPrec i 0 (concatD [prt 0 qualid, doc (showString "."), prt 0 id])

instance Print [Sit.Abs.Decl] where
  prt = prtList

instance Print Sit.Abs.IdU where
  prt i e = case e of
    Sit.Abs.Id id -> prPrec i 0 (concatD [prt 0 id])
    Sit.Abs.Under -> prPrec i 0 (concatD [doc (showString "_")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Sit.Abs.Bind where
  prt i e = case e of
    Sit.Abs.BIrrel id -> prPrec i 0 (concatD [doc (showString "."), prt 0 id])
    Sit.Abs.BRel id -> prPrec i 0 (concatD [doc (showString ".."), prt 0 id])
    Sit.Abs.BAnn ids exp -> prPrec i 0 (concatD [doc (showString "("), prt 0 ids, doc (showString ":"), prt 0 exp, doc (showString ")")])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Sit.Abs.Bind] where
  prt = prtList

instance Print [Sit.Abs.Ident] where
  prt = prtList

instance Print [Sit.Abs.IdU] where
  prt = prtList

instance Print Sit.Abs.Exp where
  prt i e = case e of
    Sit.Abs.Var idu -> prPrec i 2 (concatD [prt 0 idu])
    Sit.Abs.Int n -> prPrec i 2 (concatD [prt 0 n])
    Sit.Abs.Infty -> prPrec i 2 (concatD [doc (showString "oo")])
    Sit.Abs.Nat -> prPrec i 2 (concatD [doc (showString "Nat")])
    Sit.Abs.Set -> prPrec i 2 (concatD [doc (showString "Set")])
    Sit.Abs.Set1 -> prPrec i 2 (concatD [doc (showString "Set1")])
    Sit.Abs.Set2 -> prPrec i 2 (concatD [doc (showString "Set2")])
    Sit.Abs.Zero -> prPrec i 2 (concatD [doc (showString "zero")])
    Sit.Abs.Suc -> prPrec i 2 (concatD [doc (showString "suc")])
    Sit.Abs.Fix -> prPrec i 2 (concatD [doc (showString "fix")])
    Sit.Abs.LZero -> prPrec i 2 (concatD [doc (showString "lzero")])
    Sit.Abs.LSuc -> prPrec i 2 (concatD [doc (showString "lsuc")])
    Sit.Abs.Size -> prPrec i 0 (concatD [doc (showString "Size")])
    Sit.Abs.App exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, prt 2 exp2])
    Sit.Abs.Lam idus exp -> prPrec i 0 (concatD [doc (showString "\\"), prt 0 idus, doc (showString "->"), prt 0 exp])
    Sit.Abs.Forall binds exp -> prPrec i 0 (concatD [doc (showString "forall"), prt 0 binds, doc (showString "->"), prt 0 exp])
    Sit.Abs.Pi exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "("), prt 0 exp1, doc (showString ":"), prt 0 exp2, doc (showString ")"), doc (showString "->"), prt 0 exp3])
    Sit.Abs.Arrow exp1 exp2 -> prPrec i 0 (concatD [prt 1 exp1, doc (showString "->"), prt 0 exp2])
    Sit.Abs.Case exp1 exp2 exp3 -> prPrec i 0 (concatD [doc (showString "case"), prt 0 exp1, doc (showString "return"), prt 0 exp2, doc (showString "of"), prt 0 exp3])
    Sit.Abs.Plus exp n -> prPrec i 0 (concatD [prt 1 exp, doc (showString "+"), prt 0 n])
    Sit.Abs.ELam exp1 idu exp2 -> prPrec i 0 (concatD [doc (showString "\\"), doc (showString "{"), doc (showString "("), doc (showString "zero"), doc (showString "_"), doc (showString ")"), doc (showString "->"), prt 0 exp1, doc (showString ";"), doc (showString "("), doc (showString "suc"), doc (showString "_"), prt 0 idu, doc (showString ")"), doc (showString "->"), prt 0 exp2, doc (showString "}")])

