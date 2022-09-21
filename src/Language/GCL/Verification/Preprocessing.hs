module Language.GCL.Verification.Preprocessing where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata, para)

import Language.GCL.Syntax
import Language.GCL.Utils((...))

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