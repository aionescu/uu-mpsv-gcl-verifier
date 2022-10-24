module Language.GCL.Verification.Preprocessing where

import Control.Monad.State.Strict(State, evalState, gets, modify)
import Data.Fix(Fix(..))
import Data.Foldable(foldMap')
import Data.Functor.Foldable(cata, para)
import Data.Functor.Foldable.Monadic(cataM)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Monoid(Endo(..))

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Utils

preprocess :: Program -> Program
preprocess = runRemoveShadowing

unroll :: Int -> Expr -> Stmt -> Stmt
unroll 0 g _ = Assume' $ Not' g
unroll n g s = If' g (Seq' s $ unroll (n - 1) g s) Skip'

unrollLoops :: Int -> Program -> Program
unrollLoops k Program{..} = Program{programBody = go programBody, ..}
  where
    go = cata \case
      While g s -> unroll k g s
      s -> Fix s

runRemoveShadowing :: Program -> Program
runRemoveShadowing Program{..} =
  Program
  { programBody = removeShadowing programBody `evalState` M.empty
  , ..
  }

type Counters = Map Id Int

removeShadowing :: Stmt -> State Counters Stmt
removeShadowing = cataM \case
  Let decls s -> do
    let names = declName <$> decls
    fresh <- traverse uniqueId names

    let decls' = zipWith (\decl n -> decl{declName = n}) decls fresh
    let s' = foldMap' Endo (zipWith substSt names fresh) `appEndo` s
    pure $ Let' decls' s'
  s -> pure $ Fix s

uniqueId :: Id -> State Counters Id
uniqueId name = do
  c <- gets $ M.findWithDefault 0 name
  modify $ M.insert name $ c + 1
  pure $ "$" <> name <> showT c

substSt :: Id -> Id -> Stmt -> Stmt
substSt id id' = cata \case
  Assign i e
    | i == id -> Assign' id' $ subE e
    | otherwise -> Assign' i $ subE e
  Assert e -> Assert' $ subE e
  Assume e -> Assume' $ subE e
  AssignIndex i e1 e2
    | i == id -> AssignIndex' id' (subE e1) (subE e2)
    | otherwise -> AssignIndex' i (subE e1) (subE e2)
  If g t e -> If' (subE g) t e
  While g s -> While' (subE g) s
  p -> Fix p
  where
    subE = substId id id'

substId :: Id -> Id -> Pred -> Pred
substId id id' = para \case
  Var v | id == v -> Fix $ Var id'
  Length v | id == v -> Fix $ Length id'
  Forall v (p, _) | id == v -> Fix $ Forall v p
  Exists v (p, _) | id == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p
