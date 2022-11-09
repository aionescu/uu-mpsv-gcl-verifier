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
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

repBy :: Id -> Expr -> Expr -> Pred -> Pred
repBy a i e = cata \case
  z@(Subscript a' i') | a == a' -> IfEq' i i' e $ Fix z
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

conjunctiveWLP :: Bool -> LPath -> Pred
conjunctiveWLP (bool simplify id -> simplify') = simplify' . foldl' go T
  where
    go :: Pred -> LStmt -> Pred
    go q = \case
      LAssume e -> e :&& q
      LAssert{} -> q
      LAssign v e -> subst v e q
      LAssignIndex v i e -> repBy v i e q
