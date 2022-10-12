module Language.GCL.Verification.WLP(runWLP) where

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers (pattern T, (∧), (⟹), (¬), ifEqExpr )
import Data.Fix(Fix(..))
import Data.Functor.Foldable

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v -> e
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

repBy :: Id -> Expr -> Expr -> Pred -> Pred
repBy a i e = cata \case
  z@(Subscript a' i') | a == a' -> ifEqExpr i i' e $ Fix z
  z -> Fix z

wlp :: Stmt -> Pred -> Pred
wlp = cata \case
  Skip -> id
  Assign i e -> subst i e
  Seq s₁ s₂ -> s₁ . s₂
  Assert e -> (e ∧)
  Assume e -> (e ⟹)
  If g s₁ s₂ -> \q -> (g ⟹ s₁ q) ∧ ((¬)g ⟹ s₂ q)
  AssignIndex a i e -> repBy a i e
  Let _ s -> s
  While{} -> error "wlp: While not supported"

runWLP :: Program -> Pred
runWLP Program{..} = wlp programBody T
