module Language.GCL.Verification.Preprocessing where

import Language.GCL.Syntax
import Control.Monad.State (State, get, put, evalState)
import Data.Functor.Foldable(cata, Recursive (para))
import Data.Fix(Fix(..))
import Language.GCL.Syntax.Helpers
import Data.Functor.Foldable.Monadic (cataM)
import Language.GCL.Utils (showT)

type Counter = Int

preprocess :: Int -> Program -> Program
preprocess k = unrollLoops k . runRemoveShadowing

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assume' $ Not' g
unroll n g s = If' g (Seq' s $ unroll (n - 1) g s) Skip'

unrollLoops :: Int -> Program -> Program
unrollLoops k Program{..} = Program{programBody=cata go programBody, ..}
  where go :: StmtF Stmt -> Stmt
        go (While g s) = unroll k g s
        go p = Fix p

runRemoveShadowing :: Program -> Program
runRemoveShadowing Program{..} = Program{programBody=evalState (removeShadowing programBody) 0, ..}

removeShadowing :: Stmt -> State Counter Stmt
removeShadowing = cataM \case
  Let dcs s -> do
    let names = map (\Decl{..} -> declName) dcs
    tr <- mapM uniqueId names
    let n'' = foldl (flip $ uncurry substSt) s $ zip names tr
    let dcs' = zipWith (\Decl{..} n' -> Decl{declName=n', ..}) dcs tr
    return $ Let' dcs' n''
  st -> return $ Fix st

uniqueId :: Id -> State Counter Id
uniqueId name = do
  c <- get
  put $ c + 1
  return $ "$" <> name <> showT c

substSt :: Id -> Id -> Stmt -> Stmt
substSt id id' = cata \case
        Assign i e
          | i == id -> Assign' id' $ substId id id' e
          | otherwise -> Assign' i $ substId id id' e
        Assert e -> Assert' $ substId id id' e
        Assume e -> Assume' $ substId id id' e
        AssignIndex i e1 e2
          | i == id -> AssignIndex' id' (substId id id' e1) (substId id id' e2)
          | otherwise -> AssignIndex' i (substId id id' e1) (substId id id' e2)
        If g t e -> If' (substId id id' g) t e
        While g s -> While' (substId id id' g) s
        p -> Fix p

substId :: Id -> Id -> Pred -> Pred
substId id id' = para \case
  Var v | id == v -> Fix $ Var id'
  Length v | id == v -> Fix $ Length id'
  Forall v (p, _) | id == v -> Fix $ Forall v p
  Exists v (p, _) | id == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p
