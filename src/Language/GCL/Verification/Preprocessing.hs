module Language.GCL.Verification.Preprocessing where

import Language.GCL.Syntax
import Control.Monad.State (State, get, put, evalState)
import Data.Functor.Foldable(cata, Recursive (para))
import Data.Fix(Fix(..))
import Language.GCL.Syntax.Helpers ( ifSt, skipSt, seqSt, (¬), assertSt, letSt, assignSt, assumeSt, assignIndexSt ) 
import Data.Functor.Foldable.Monadic (cataM)
import Language.GCL.Utils (showT)

type Counter = Int

preprocess :: Program -> Program
preprocess = unrollLoops . runRemoveShadowing

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = assertSt . (¬) $ g
unroll n g s = ifSt g (seqSt s $ unroll (n - 1) g s) skipSt


unrollLoops :: Program -> Program
unrollLoops Program{..} = Program{programBody=cata go programBody, ..}
  where go :: StmtF Stmt -> Stmt
        go (While g s) = unroll 10 g s
        go p = Fix p


runRemoveShadowing :: Program -> Program
runRemoveShadowing Program{..} = Program{programBody=evalState (removeShadowing programBody) 0, ..} 


removeShadowing :: Stmt -> State Counter Stmt
removeShadowing = cataM go
  where go :: StmtF Stmt -> State Counter Stmt
        go (Let dcs s) = do
            ns <- removeShadowing s
            let names = map (\Decl{..} -> declName) dcs
            tr <- mapM uniqueId names
            let nS = foldl (flip $ uncurry substSt) ns $ zip names tr
            return nS
        go st = return $ Fix st

uniqueId :: Id -> State Counter Id
uniqueId name = do
  c <- get
  put $ c + 1
  return $ "$" <> name <> showT c

substSt :: Id -> Id -> Stmt -> Stmt 
substSt id id' = cata \case
        Assign i e | i == id -> assignSt id' $ subst id id' e
        Assign i e -> assignSt i $ subst id id' e
        Assert e -> assertSt $ subst id id' e
        Assume e -> assumeSt $ subst id id' e
        AssignIndex i e1 e2 | i == id -> assignIndexSt id' (subst id id' e1) (subst id id' e2)
        p -> Fix p 

subst :: Id -> Id -> Pred -> Pred
subst id id' = para \case
  Var v | id == v -> Fix $ Var id'
  Length v | id == v -> Fix $ Length id'
  Forall v (p, _) | id == v -> Fix $ Forall v p
  Exists v (p, _) | id == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p
