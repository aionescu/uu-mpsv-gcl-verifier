module Language.GCL.Verification(runWLP) where

import Data.Coerce(coerce)
import Data.Fix(Fix(..))

import Language.GCL.Syntax

subst :: Id -> Expr -> Pred -> Pred
subst i e = coerce \case
  Var x | i == x -> unFix e
  Length x | i == x -> unFix e
  BinOp op lhs rhs -> BinOp op (subst i e lhs) (subst i e rhs)
  Negate rhs -> Negate $ subst i e rhs
  Subscript v s -> Subscript v $ subst i e s
  Forall v p | i /= v -> Forall v $ subst i e p
  Exists v p | i /= v -> Exists v $ subst i e p
  p -> p

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assert (-g)
unroll n g s = If g (Seq s $ unroll (n - 1) g s) Skip

wlp :: Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp s₁ $ wlp s₂ q
wlp (Assert e) q = e :&& q
wlp (Assume e) q = e :=> q
wlp (If g s₁ s₂) q = (g :=> wlp s₁ q) :&& (-g :=> wlp s₂ q)
wlp (While g s) q = wlp (unroll 10 g s) q
wlp _ _ = error "wlp: TODO"

runWLP :: Program -> Pred
runWLP Program{..} = wlp programBody true
