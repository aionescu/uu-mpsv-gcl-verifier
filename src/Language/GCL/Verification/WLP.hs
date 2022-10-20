module Language.GCL.Verification.WLP(wlp) where

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Data.Fix(Fix(..))
import Data.Functor.Foldable
import Language.GCL.Verification.Simplification (simplify)

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

wlp :: Bool -> Program -> [Pred]
wlp noSimplify Program{..} = prune $ go programBody [T]
  where
    prune
      | noSimplify = id
      | otherwise = filter (\case T -> False; _ -> True) . fmap simplify

    go :: Stmt -> [Pred] -> [Pred]
    go = cata \case
      Skip -> id
      Assign i e -> fmap $ subst i e
      Seq s1 s2 -> s1 . s2
      Assert e -> fmap (e :&&)
      Assume e -> fmap (e :=>)
      If g s1 s2 -> \q -> prune $ fmap (g :=>) (s1 q) <> fmap (Not' g :=>) (s2 q)
      AssignIndex a i e -> fmap $ repBy a i e
      Let _ s -> s
      While{} -> error "wlp: Loops should have been eliminated by preprocessing"
