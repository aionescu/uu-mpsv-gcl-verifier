module Language.GCL.Verification.Linearization where

import Control.Monad(join)
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
import Language.GCL.Verification.WLP
import Language.GCL.Verification.Z3

type Counters = Map Id Int

removeShadowing :: Stmt -> Stmt
removeShadowing = flip evalState M.empty . cataM \case
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

linearize :: Bool -> Int -> Program -> IO [LPath]
linearize noHeuristics maxDepth program@Program{..} = reverse <$> go maxDepth p [] (const pure)
  where
    p = removeShadowing programBody
    vars = collectVars program

    prune'
      | noHeuristics = go
      | otherwise = prune

    prune :: Int -> Stmt -> LPath -> (Int -> [LPath] -> IO [LPath]) -> IO [LPath]
    prune d _ _ k | d < 0 = k d []
    prune d s p k =
      checkSAT vars (conjunctiveWLP noHeuristics p) >>= \case
        True -> go d s p k
        _ -> k d []

    go :: Int -> Stmt -> LPath -> (Int -> [LPath] -> IO [LPath]) -> IO [LPath]
    go d _ _ k | d < 0 = k d []
    go d s p k = case unFix s of
      Skip -> k d [p]
      Assume e -> k (d - 1) [LAssume e : p]
      Assert e -> k (d - 1) [LAssert e : p]
      Assign v e -> k (d - 1) [LAssign v e : p]
      AssignIndex v i e -> k (d - 1) [LAssignIndex v i e : p]
      If g t e -> prune' (d - 1) t (LAssume g : p) k <> prune' (d - 1) e (LAssume (Not' g) : p) k
      While g body -> go d (If' g (Seq' body s) Skip') p k
      Seq a b -> go d a p \d ps -> join <$> traverse (\p -> go d b p k) ps
      Let _ s -> go d s p k
