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

substSubscript :: Id -> Expr -> Expr -> Pred -> Pred
substSubscript id c e' = cata \case
  Subscript i e | i == id -> ifEqExpr e c e' $ Fix (Subscript i e)
  z -> Fix z

wlp :: StmtF Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp (unFix s₁) $ wlp (unFix s₂) q
wlp (Assert e) q = e ∧ q
wlp (Assume e) q = e  ⟹ q
wlp (If g s₁ s₂) q = (g ⟹ wlp (unFix s₁) q) ∧ ((¬)g ⟹ wlp (unFix s₂) q)
wlp (AssignIndex ad i as) q = substSubscript ad i as q
wlp s _ = error $ "WLP not yet implemented for: " <> show (Fix s)

runWLP :: Program -> Pred
runWLP Program{..} = wlp (unFix programBody) T