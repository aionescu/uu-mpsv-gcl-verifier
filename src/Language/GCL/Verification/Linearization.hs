module Language.GCL.Verification.Linearization where

import Control.Monad.State.Lazy(StateT, runStateT, get, put, liftIO)
import Data.Fix(Fix(..))
import Data.Functor.Foldable(para)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Utils
import Language.GCL.Verification.WLP
import Language.GCL.Verification.Z3
import Control.Monad (join)

type Collected = (Map Id Int, Map Id Id, Map Id Type)
type Linearizer a = StateT Collected IO a

addShadowed :: [Decl] -> Linearizer ()
addShadowed = mapM_ \Decl{..} -> uniqueId declName declType

uniqueId :: Id -> Type -> Linearizer Id
uniqueId name tp = do
  (ns, ct, vs) <- get
  let c = M.findWithDefault 0 name ns
  let name' = "$" <> name <> showT c

  let ns' = M.insert name (c + 1) ns
  let vs' = M.insert name' tp vs
  let ct' = M.insert name name' ct
  put (ns', ct', vs')

  pure name'

linearize :: Bool -> Int -> Program -> IO ([LPath], Map Id Type)
linearize noHeuristics maxDepth program@Program{..} = do
  let progVars = collectVars program
  let lin = go maxDepth programBody [] (const pure)
  (res, (_, _, vars)) <- runStateT lin (M.empty, M.empty, progVars)
  pure (reverse <$> res, vars)
  where
    prune'
      | noHeuristics = go
      | otherwise = prune

    prune :: Int -> Stmt -> LPath -> (Int -> [LPath] -> Linearizer [LPath]) -> Linearizer [LPath]
    prune d _ p k | d < 0 = k d [p]
    prune d s p k = do
      (_, _, vs) <- get
      feasible <- liftIO $ checkSAT vs $ conjunctiveWLP noHeuristics $ reverse p
      (if feasible then go d s p k else pure [])

    go :: Int -> Stmt -> LPath -> (Int -> [LPath] -> Linearizer [LPath]) -> Linearizer [LPath]
    go d _ p k | d < 0 = k d [p]
    go d s p k = case unFix s of
      Skip -> k d []
      Assume e -> do
        u <- unshadow $ LAssume e
        k (d - 1) [u : p]
      Assert e -> do
        u <- unshadow $ LAssert e
        k (d - 1) [u : p]
      Assign v e -> do
        u <- unshadow $ LAssign v e
        k (d - 1) [u : p]
      AssignIndex v i e -> do
        u <- unshadow $ LAssignIndex v i e
        k (d - 1) [u : p]
      AssignNew v e -> do
        u <- unshadow $ LAssignNew v e
        k (d - 1) [u : p]
      AssignVal v e -> do
        u <- unshadow $ LAssignVal v e
        k (d - 1) [u : p]
      If g s1 s2 -> do
        u1 <- unshadow $ LAssume g
        u2 <- unshadow $ LAssume (Not' g)
        t <- prune' (d - 1) s1 (u1 : p) k
        f <- prune' (d - 1) s2 (u2 : p) k
        pure $ t <> f
      While g body -> go d (If' g (Seq' body s) Skip') p k
      Seq a b -> go d a p \d ps -> join <$> traverse (\p -> go d b p k) ps
      Let ds s -> do
        prev <- get
        addShadowed ds
        r <- go d s p k
        put prev
        pure r

substId :: Id -> Id -> Pred -> Pred
substId id id' = para \case
  Var v | id == v -> Fix $ Var id'
  GetVal v | id == v -> Fix $ GetVal id'
  Length v | id == v -> Fix $ Length id'
  Forall v (p, _) | id == v -> Fix $ Forall v p
  Exists v (p, _) | id == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p

unshadow :: LStmt -> Linearizer LStmt
unshadow st = do
  (_, ct, _) <- get
  let all = M.toList ct
  return $ foldl (flip $ uncurry rep) st all
  where
    rep id id' = \case
      LAssume e -> LAssume $ substId id id' e
      LAssert e -> LAssert $ substId id id' e
      LAssign i e -> LAssign (if i == id then id' else i) $ substId id id' e
      LAssignIndex i e1 e2 -> LAssignIndex (if i == id then id' else i) (substId id id' e1) (substId id id' e2)
      LAssignNew i e -> LAssignNew (if i == id then id' else i) $ substId id id' e
      LAssignVal i e -> LAssignVal (if i == id then id' else i) $ substId id id' e
