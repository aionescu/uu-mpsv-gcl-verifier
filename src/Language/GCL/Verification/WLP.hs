module Language.GCL.Verification.WLP(wlp, conjunctiveWLP) where

import Data.Bool(bool)
import Data.Fix(Fix(..))
import Data.Foldable(foldl')
import Data.Functor.Foldable(cata, para)

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Verification.Simplification(simplify)

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v -> e
  GetVal v | Fix (Var e') <- e, i == v -> Fix $ GetVal e'
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

repBy :: Id -> Expr -> Expr -> Pred -> Pred
repBy a i e = cata \case
  z@(Subscript a' i') | a == a' -> Conditional' (i :== i') e $ Fix z
  z -> Fix z

substNew :: Id -> Expr -> Pred -> Pred
substNew i e = cata \case
  GetVal i' | i == i' -> e
  p -> Fix p

substVal :: Id -> Expr -> Pred -> Pred
substVal (Var' -> i) e = cata \case
  z@(GetVal (Var' -> v)) -> Conditional' ((i :!= Null') :&& (i :== v)) e $ Fix z
  z -> Fix z

wlp :: Bool -> LPath -> Pred
wlp (bool simplify id -> simplify') = simplify' . foldr go T
  where
    go :: LStmt -> Pred -> Pred
    go = \case
      LAssume e -> (e :=>)
      LAssert e -> (e :&&)
      LAssign v e -> subst v e
      LAssignIndex v i e -> repBy v i e
      LAssignNew v e ->  (\p -> Var' v :!= Null' :=> substNew v e p)
      LAssignVal v e -> substVal v e

conjunctiveWLP :: Bool -> LPath -> Pred
conjunctiveWLP (bool simplify id -> simplify') = simplify' . foldl' go T
  where
    go :: Pred -> LStmt -> Pred
    go q = \case
      LAssume e -> e :&& q
      LAssert{} -> q
      LAssign v e -> subst v e q
      LAssignIndex v i e -> repBy v i e q
      LAssignNew v e -> substNew v e q
      LAssignVal v e -> substVal v e q
