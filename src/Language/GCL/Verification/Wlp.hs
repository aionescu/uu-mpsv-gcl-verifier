module Language.GCL.Verification.Wlp(runWLP) where

import Language.GCL.Syntax
import Data.Functor.Foldable(para)
import Language.GCL.Verification.Helpers (pattern T, (∧), (∨), (⟹), (¬) )
import Data.Fix(Fix(..))

subst :: Id -> Expr -> Pred -> Pred
subst i e = para \case
  Var v | i == v -> e
  Length v | i == v -> e
  Forall v (p, _) | i == v -> Fix $ Forall v p
  Exists v (p, _) | i == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

wlp :: StmtF Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp (unFix s₁) $ wlp (unFix s₂) q
wlp (Assert e) q = e ∧ q
wlp (Assume e) q = e ∨ q
wlp (If g s₁ s₂) q = (g ⟹ wlp (unFix s₁) q) ∧ ((¬)g ⟹ wlp (unFix s₂) q)
wlp _ _ = error "WLP not yet implemented"
-- wlp s _ = error $ "WLP not yet implemented for: " <> show (show s)

runWLP :: Program -> Pred
runWLP Program{..} = wlp (unFix programBody) T