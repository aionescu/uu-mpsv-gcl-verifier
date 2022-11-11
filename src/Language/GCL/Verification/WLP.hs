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

repAddress :: Id -> Int -> Pred -> Pred
repAddress v (I -> add) = subst v add

wlp :: Bool -> LPath -> Pred
wlp (bool simplify id -> simplify') path = simplify' . fst $ foldr go (T, 1) path
  where
    go :: LStmt -> (Pred, Int) -> (Pred, Int)
    go p (q, idx) = case p of
      LAssume e -> (e :=> q, idx)
      LAssert e -> (e :&& q, idx)
      LAssign v e -> (subst v e q, idx)
      LAssignIndex v i e -> (repBy v i e q, idx)
      LAssignNew v e ->  (Var' v :!= Null' :=> repAddress v idx (substNew v e q), idx + 1)
      LAssignVal v e -> (substVal v e q, idx)


conjunctiveWLP :: Bool -> LPath -> Pred
conjunctiveWLP (bool simplify id -> simplify') path = simplify' . fst $ foldl' go (T, 1) path
    where
      go :: (Pred, Int) -> LStmt -> (Pred, Int)
      go (q, idx) = \case
          LAssume e -> (e :&& q, idx)
          LAssert{} -> (q, idx)
          LAssign v e -> (subst v e q, idx)
          LAssignIndex v i e -> (repBy v i e q, idx)
          LAssignNew v e -> (Var' v :!= Null' :=> repAddress v idx (substNew v e q), idx + 1)
          LAssignVal v e -> (substVal v e q, idx)


