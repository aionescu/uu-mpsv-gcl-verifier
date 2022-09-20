module Language.GCL.Verification(runWLP) where

import Language.GCL.Syntax

subst :: Id -> Expr -> Pred -> Pred
subst _ _ l@(IntLit _) = l
subst _ _ l@(BoolLit _) = l
subst i e v@(Var x)
  | i == x = e
  | otherwise = v
subst i e v@(Length x)
  | i == x = e
  | otherwise = v
subst i e (BinOp op lhs rhs) = BinOp op (subst i e lhs) (subst i e rhs)
subst i e (Negate rhs) = Negate $ subst i e rhs
subst i e (Subscript v s) = Subscript v $ subst i e s
subst i e f@(Forall v p)
  | i == v = f
  | otherwise = Forall v $ subst i e p
subst i e f@(Exists v p)
  | i == v = f
  | otherwise = Exists v $ subst i e p

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
runWLP Program{..} = wlp programBody $ BoolLit True
