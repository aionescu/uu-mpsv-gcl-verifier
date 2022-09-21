{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.GCL.Verification where

import Language.GCL.Syntax
import Control.Monad.State.Strict(State, get, put, execState)
import Control.Monad(foldM)
import Language.GCL.Utils

type Counter = Int

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
-- wlp (Let ds sts) q = do
--   nNm <- mapM fresh ds
  
wlp _ _ = error "wlp: TODO"


fresh :: Id -> State Counter Id 
fresh name = do
  c <- get
  put $ c + 1
  return $ "$" <> name <> (showT c)

substStmt :: Id -> Id -> Stmt -> State Counter Stmt
substStmt id nid (Assign i e) = return $ Assign cid $ subst id (Var nid) e
  where cid = if i == id then nid else i
substStmt id nid (Seq s1 s2) = do
  l <- substStmt id nid s2
  r <- substStmt id nid s1
  return $ Seq l r
substStmt id nid (Assert e) = return $ Assert $ subst id (Var nid) e
substStmt id nid (Assume e) = return $ Assume $ subst id (Var nid) e
substStmt id nid (AssignIndex i e1 e2) = return $ AssignIndex cid (subst id (Var nid) e1) (subst id (Var nid) e2)
  where cid = if i == id then nid else i
substStmt id nid (Let _ s) = substStmt id nid s


removeShadowingStmt :: Stmt -> State Counter Stmt
removeShadowingStmt (Let dcs s) = do
  ns <- removeShadowingStmt s
  let names = map (\Decl{..} -> declName) dcs
  tr <- mapM fresh names
  let nD = map (\(Decl{..}, n) -> Decl{declName=n, declType}) $ zip dcs tr
  nS <- foldM (flip $ uncurry substStmt) ns $ zip names tr
  return $ Let nD nS


removeShadowingExec :: Program -> State Counter Program
removeShadowingExec Program{..} = do
  programBody <- removeShadowingStmt programBody
  return Program{..}

removeShadowing :: Program -> Program
removeShadowing p = execState (removeShadowingExec p) 0

-- Program{programBody= {} , ..}
-- p {programBody = {}}

