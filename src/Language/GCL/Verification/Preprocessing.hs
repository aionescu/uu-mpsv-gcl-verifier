{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Language.GCL.Verification.Preprocessing where

import Language.GCL.Syntax
import Control.Monad.State (State, get, put, evalState)
import Control.Monad (foldM)
import Data.Functor.Foldable(cata)
import Data.Fix(Fix(..))
import Language.GCL.Verification.Helpers ( ifSt, skipSt, seqSt, (¬), assertSt, letSt ) 
import Data.Functor.Foldable.Monadic (cataM)
import Language.GCL.Utils (showT)

type Counter = Int

preprocess :: Program -> Program
preprocess = unrollLoops

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = assertSt . (¬) $ g
unroll n g s = ifSt g (seqSt s $ unroll (n - 1) g s) skipSt


unrollLoops :: Program -> Program
unrollLoops Program{..} = Program{programBody=cata go programBody, ..}
  where go :: StmtF Stmt -> Stmt
        go (While g s) = unroll 10 g s
        go p = Fix p


-- runRemoveShadowing :: Program -> Program
-- runRemoveShadowing Program{..} = Program{programBody=evalState (removeShadowing programBody) 0, ..} 


-- z :: (Monad m, Traversable (Data.Functor.Foldable.Base t),
--  Data.Functor.Foldable.Recursive t) =>
-- (Data.Functor.Foldable.Base t a -> m a) -> t -> m a
-- z = cataM

-- removeShadowing :: Stmt -> State Counter Stmt
-- removeShadowing = cataM go
--   where go :: StmtF Stmt -> State Counter Stmt
--         -- go (Let dcs s) = return $ letSt dcs s
--             -- ns <- removeShadowing s
--             -- let names = map (\Decl{..} -> declName) dcs
--             -- tr <- mapM fresh names
--             -- let nD = zipWith (curry (\(Decl{..}, n) -> Decl{declName=n, declType})) dcs tr
--             -- nS <- foldM (flip $ uncurry substStmt) ns $ zip names tr
--             -- return $ letSt nD nS
            
--         go st = return $ Fix st

-- substStmt :: Id -> Id -> Stmt -> State Counter Stmt 
-- substStmt id id' st = cataM go st
--   where go :: StmtF Stmt -> State Counter Stmt
--         go st = return $ Fix st
  
        -- go (Let dcs s) = do
        --     ns <- removeShadowing s
        --     let names = map (\Decl{..} -> declName) dcs
        --     tr <- mapM fresh names
        --     let nD = zipWith (curry (\(Decl{..}, n) -> Decl{declName=n, declType})) dcs tr
        --     nS <- foldM (flip $ uncurry substStmt) ns $ zip names tr
        --     return $ letSt nD nS




-- (Let dcs s) = do


-- fresh :: Id -> State Counter Id
-- fresh name = do
--   c <- get
--   put $ c + 1
--   return $ "$" <> name <> showT c

-- where go :: StmtF Stmt -> Stmt
--       go () =  



-- substStmt :: Id -> Id -> Stmt -> State Counter Stmt
-- substStmt id nid (Assign i e) = return $ Assign cid $ subst id (Var nid) e
--   where cid = if i == id then nid else i
-- substStmt id nid (Seq s1 s2) = do
--   l <- substStmt id nid s2
--   r <- substStmt id nid s1
--   return $ Seq l r
-- substStmt id nid (Assert e) = return $ Assert $ subst id (Var nid) e
-- substStmt id nid (Assume e) = return $ Assume $ subst id (Var nid) e
-- substStmt id nid (AssignIndex i e1 e2) = return $ AssignIndex cid (subst id (Var nid) e1) (subst id (Var nid) e2)
--   where cid = if i == id then nid else i
-- substStmt id nid (Let _ s) = substStmt id nid s

-- removeShadowingStmt :: Stmt -> State Counter Stmt
-- removeShadowingStmt (Let dcs s) = do

-- removeShadowing :: Program -> State Counter Program
-- removeShadowing Program{..} = do
--   programBody <- removeShadowingStmt programBody
--   return Program{..}


