module Language.GCL.Verification where

import Language.GCL.Syntax

subst :: Id -> Expr -> Pred -> Pred
subst _ _ l@(IntLit _) = l
subst _ _ l@(BoolLit _) = l
subst i e (Var v)
  | i == v = e
  | otherwise = Var v
subst i e (Length v)
  | i == v = e
  | otherwise = Length v
subst i e (BinOp lhs op rhs) = BinOp (subst i e lhs) op (subst i e rhs)
subst i e (Negate rhs) = Negate $ subst i e rhs
subst i e (Subscript v s) = Subscript v $ subst i e s
subst i e f@(Forall v p)
  | i == v = f
  | otherwise = Forall v $ subst i e p
subst i e f@(Exists v p)
  | i == v = f
  | otherwise = Exists v $ subst i e p

wlp :: Stmt -> Pred -> Pred
wlp Skip q = q
wlp (Assign i e) q = subst i e q
wlp (Seq s₁ s₂) q = wlp s₁ $ wlp s₂ q
wlp (Assert e) q = BinOp e And q
wlp (Assume e) q = BinOp e Implies q
wlp _ _ = error "wlp: TODO"
