module Language.GCL.Verification.WLP(wlp, conjunctiveWLP) where

import Data.Bool(bool)
import Data.Fix(Fix(..))
import Data.Foldable(foldl')
import Data.Functor.Foldable(para)

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Verification.Simplification(simplify)

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v, Fix (Var e') <- e -> Fix $ Length e'
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

wlp :: Bool -> LPath -> Pred
wlp (bool simplify id -> simplify') = simplify' . foldl' go T
  where
    go :: Pred -> LStmt -> Pred
    go q = \case
      LAssume e -> e :=> q
      LAssert e -> e :&& q
      LAssign v e -> subst v e q
      LAssignIndex v i e -> subst v (RepBy' (Var' v) i e) q

conjunctiveWLP :: Bool -> LPath -> Pred
conjunctiveWLP (bool simplify id -> simplify') = simplify' . foldl' go T
  where
    go :: Pred -> LStmt -> Pred
    go q = \case
      LAssume e -> e :&& q
      LAssert{} -> q
      LAssign v e -> subst v e q
      LAssignIndex v i e -> subst v (RepBy' (Var' v) i e) q
