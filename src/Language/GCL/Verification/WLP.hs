module Language.GCL.Verification.WLP(runWLP) where

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Data.Fix(Fix(..))
import Data.Functor.Foldable
import Language.GCL.Verification.Simplification (simplify)
import Data.Bool (bool)

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

reduce:: [Pred] -> [Pred]
reduce = filter (/= T) . map simplify

wlp :: Bool -> Stmt -> [Pred] -> [Pred]
wlp smp = cata \case
  Skip -> id
  Assign i e -> map $ subst i e
  Seq s1 s2 -> s1 . s2
  Assert e -> map (e :&&)
  Assume e -> map (e :=>)
  If g s1 s2 -> \q -> bool id reduce smp $ map (g :=>) (s1 q) <> map (Not' g :=>) (s2 q)
  AssignIndex a i e -> map $ repBy a i e
  Let _ s -> s
  While{} -> error "wlp: While not supported"

runWLP :: Program -> [Pred]
runWLP Program{..} = wlp True programBody [T]
