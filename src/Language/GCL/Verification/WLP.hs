module Language.GCL.Verification.WLP(wlp) where

import Data.Fix(Fix(..))
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

wlp :: Bool -> Int -> Program -> [Pred]
wlp noSimplify maxDepth Program{..} = prune $ go maxDepth programBody [T] $ const id
  where
    go :: Int -> Stmt -> [Pred] -> (Int -> [Pred] -> k) -> k
    go d _ _ k | d < 0 = k d []
    go d s q k = case unFix s of
      Skip -> k (d - 1) q
      Assume e -> k (d - 1) $ (e :=>) <$> q
      Assert e -> k (d - 1) $ (e :&&) <$> q
      Assign v e -> k (d - 1) $ subst v e <$> q
      AssignIndex v i e -> k (d - 1) $ repBy v i e <$> q
      If g t e ->
        go (d - 1) t q \_ t ->
          go (d - 1) e q \_ e ->
            k (d - 1) $ prune $ fmap (g :=>) t <> fmap (Not' g :=>) e
      While g s ->
        let s' = If' g (Seq' s s') Skip'
        in go d s' q k
      Seq a b -> go d b q \d q -> go d a q k
      Let _ s -> go d s q k

    prune :: [Pred] -> [Pred]
    prune
      | noSimplify = id
      | otherwise = filter (\case T -> False; _ -> True) . fmap simplify
