module Language.GCL.Verification.WLP(runWLP) where

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers (pattern T, (∧), (⟹), (¬), ifEqExpr )
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
  z@(Subscript a' i') | a == a' -> ifEqExpr i i' e $ Fix z
  z -> Fix z

reduce:: [Pred] -> [Pred]
reduce = filter (/= T) . map simplify

wlp :: Bool -> Stmt -> [Pred] -> [Pred]
wlp smp = cata \case
  Skip -> id
  Assign i e -> map $ subst i e
  Seq s₁ s₂ -> s₁ . s₂
  Assert e -> map (e ∧)
  Assume e -> map (e ⟹)
  If g s₁ s₂ -> \q -> simplify $ map (g ⟹) (s₁ q) <> map ((¬)g ⟹) (s₂ q)
  AssignIndex a i e -> map $ repBy a i e
  Let _ s -> s
  While{} -> error "wlp: While not supported"
  where simplify = if smp then reduce else id

runWLP :: Program -> [Pred]
runWLP Program{..} = wlp True programBody [T]
