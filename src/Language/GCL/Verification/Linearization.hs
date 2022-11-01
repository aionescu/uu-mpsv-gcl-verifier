module Language.GCL.Verification.Linearization where

import Control.Monad(foldM, when)
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

type Collected = (Map Id Int, Map Id Type)
type Linearlizer a = StateT Collected IO a

removeShadowing :: [Decl] -> LPath -> Linearlizer LPath
removeShadowing ds ps = foldM remove ps ds
  where remove :: LPath -> Decl -> Linearlizer LPath
        remove ps Decl{..} = do
          id' <- uniqueId declName declType
          return $ map (subLs declName id') ps

        subLs id id' = \case
          LAssume e -> LAssume $ substId id id' e
          LAssert e -> LAssert $ substId id id' e
          LAssign i e
            | i == id -> LAssign id' $ substId id id' e
            | otherwise -> LAssign i $ substId id id' e
          LAssignIndex i e1 e2
            | i == id -> LAssignIndex id' (substId id id' e1) (substId id id' e2)
            | otherwise -> LAssignIndex i (substId id id' e1) (substId id id' e2)



uniqueId :: Id -> Type -> Linearlizer Id
uniqueId name tp = do
  (ns, vs) <- get
  let c = M.findWithDefault 0 name ns
  let name' = "$" <> name <> showT c
  let ns' = M.insert name (c + 1) ns
  let vs' = M.insert name' tp vs
  put (ns', vs')
  pure name'


substId :: Id -> Id -> Pred -> Pred
substId id id' = para \case
  Var v | id == v -> Fix $ Var id'
  Length v | id == v -> Fix $ Length id'
  Forall v (p, _) | id == v -> Fix $ Forall v p
  Exists v (p, _) | id == v -> Fix $ Exists v p
  p -> Fix $ snd <$> p


linearize :: Bool -> Int -> Program -> IO ([LPath], Map Id Type)
linearize noHeuristics maxDepth program@Program{..} = do
  let progVars = collectVars program
  let lin = go maxDepth programBody [] (const pure)
  (res, (_, vars)) <- runStateT lin (M.empty, progVars)
  return (res, vars)
  where
    prune'
      | noHeuristics = go
      | otherwise = prune

    prune :: Int -> Stmt -> LPath -> (Int -> [LPath] -> Linearlizer [LPath]) -> Linearlizer [LPath]
    prune d _ _ k| d < 0 = k d []
    prune d s p k = do
      (_, vs) <- get
      checked <- liftIO $ checkSAT vs (conjunctiveWLP noHeuristics p)
      (if checked then go d s p k else return [])

    go :: Int -> Stmt -> LPath -> (Int -> [LPath] -> Linearlizer [LPath]) -> Linearlizer [LPath]
    go d _ _ k | d < 0 = k d []
    go d s p k = case unFix s of
      Skip -> k d [p]
      Assume e -> k d [LAssume e : p]
      Assert e -> k d [LAssert e : p]
      Assign v e -> k d [LAssign v e : p]
      AssignIndex v i e -> k d [LAssignIndex v i e : p]
      If g s1 s2 -> do
        t <- prune' (d - 1) s1 (LAssume g : p) k
        f <- prune' (d - 1) s2 (LAssume (Not' g) : p) k
        return $ t <> f
      While g body -> go d (If' g (Seq' body s) Skip') p k
      Seq a b -> do
        l <- go d a p k
        r <- mapM (\lp -> go d b lp k) l
        return $ concat r
      Let ds s -> do
        z <- go d s p k
        mapM (removeShadowing ds) z

combine:: [LPath] -> [LPath] -> [LPath]
combine l r = [ x <> y | x <- l, y <- r ]