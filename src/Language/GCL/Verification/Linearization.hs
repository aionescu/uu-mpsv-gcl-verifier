module Language.GCL.Verification.Linearization where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(para)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Utils
import Language.GCL.Verification.WLP
import Language.GCL.Verification.Z3
import Data.Functor ((<&>))
import Data.List (unzip4)

substMap :: Map Id Id -> Pred -> Pred
substMap m = para \case
  Var v -> Fix $ Var $ M.findWithDefault v v m
  GetVal v -> Fix $ GetVal $ M.findWithDefault v v m
  Length v -> Fix $ Length $ M.findWithDefault v v m
  Forall v (p, _) | M.member v m -> Fix $ Forall v $ substMap (M.delete v m) p
  Exists v (p, _) | M.member v m -> Fix $ Exists v $ substMap (M.delete v m) p
  p -> Fix $ snd <$> p

substShadowed :: Map Id Id -> Stmt -> Stmt
substShadowed m = para \case
  Skip -> Skip'
  Assume e -> Assume' $ substMap m e
  Assert e -> Assert' $ substMap m e
  Assign v e -> Assign' (M.findWithDefault v v m) $ substMap m e
  AssignIndex v i e -> AssignIndex' (M.findWithDefault v v m) (substMap m i) $ substMap m e
  AssignNew v e -> AssignNew' (M.findWithDefault v v m) $ substMap m e
  AssignVal v e -> AssignVal' (M.findWithDefault v v m) $ substMap m e
  If g (_, t) (_, e) -> If' (substMap m g) t e
  While g (_, b) -> While' (substMap m g) b
  Seq (_, a) (_, b) -> Seq' a b
  Let ds (e, _) -> Let' ds $ substShadowed (foldr M.delete m $ declName <$> ds) e

linearize :: Bool -> Int -> Program -> [(Map Id Type, LPath)]
linearize noHeuristics maxDepth Program{..} =
  (\(_, _, tys, p) -> (tys, reverse p))
  <$> go maxDepth M.empty initialTys [] programBody
  where
    initialTys = M.fromList $ (\Decl{..} -> (declName, declType)) <$> programOutput : programInputs

    prune :: Int -> Map Id Int -> Map Id Type -> LPath -> Stmt -> [(Int, Map Id Int, Map Id Type, LPath)]
    prune d ids tys p s
      | d < 0 = []
      | noHeuristics || unsafeCheckSAT tys (conjunctiveWLP noHeuristics p) = go d ids tys p s
      | otherwise = []

    go :: Int -> Map Id Int -> Map Id Type -> LPath -> Stmt -> [(Int, Map Id Int, Map Id Type, LPath)]
    go d _ _ _ _ | d < 0 = []
    go d ids tys p s =
      case unFix s of
        Skip -> [(d, ids, tys, p)]
        Assume e -> [(d - 1, ids, tys, LAssume e : p)]
        Assert e -> [(d - 1, ids, tys, LAssert e : p)]
        Assign v e -> [(d - 1, ids, tys, LAssign v e : p)]
        AssignIndex v i e -> [(d - 1, ids, tys, LAssignIndex v i e : p)]
        AssignNew v e -> [(d - 1, ids, tys, LAssignNew v e : p)]
        AssignVal v e -> [(d - 1, ids, tys, LAssignVal v e : p)]
        If g t e -> prune (d - 1) ids tys (LAssume g : p) t <> prune (d - 1) ids tys (LAssume (Not' g) : p) e
        While g b -> go d ids tys p $ If' g (Seq' b s) Skip'
        Seq a b -> go d ids tys p a >>= \(d, ids, tys, p) -> go d ids tys p b
        Let ds s -> go d ids' tys' p $ substShadowed (M.fromList $ zip names names') s
          where
            ids' = M.fromList (zip names $ (+ 1) <$> ixs) <> ids
            tys' = M.fromList (zip names' types) <> tys
            (names, types, ixs, names') = unzip4 $ ds <&> \Decl{..} ->
              let ix = M.findWithDefault 0 declName ids
              in (declName, declType, ix, "$" <> declName <> showT ix)
