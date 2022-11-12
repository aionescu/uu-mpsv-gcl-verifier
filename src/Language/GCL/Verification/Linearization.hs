module Language.GCL.Verification.Linearization where

import Control.Monad(join)
import Data.Fix(Fix(..))
import Data.Functor((<&>))
import Data.Functor.Foldable(para)
import Data.List(unzip4)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Utils
import Language.GCL.Verification.WLP
import Language.GCL.Verification.Z3

substMap :: Map Id Id -> Pred -> Pred
substMap m = para \case
  Var v -> Fix $ Var $ M.findWithDefault v v m
  Length v -> Fix $ Length $ M.findWithDefault v v m
  Forall v (p, _) | M.member v m -> Fix $ Forall v $ substMap (M.delete v m) p
  Exists v (p, _) | M.member v m -> Fix $ Exists v $ substMap (M.delete v m) p
  p -> Fix $ snd <$> p

substMapStmt :: Map Id Id -> Stmt -> Stmt
substMapStmt m = para \case
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
  Let ds (e, _) -> Let' ds $ substMapStmt (foldr M.delete m $ declName <$> ds) e

linearizeStmt :: Bool -> Bool -> Int -> Int -> [Decl] -> Stmt  -> IO [(Map Id Type, LPath)]
linearizeStmt noPrune noSimplify maxDepth firstPtr decls s =
  ((\(_, _, _, tys, p) -> (tys, p)) <$>) <$> go maxDepth firstPtr M.empty initialTys [] s
  where
    initialTys = M.fromList $ (\Decl{..} -> (declName, declType)) <$> decls

    prune :: Int -> Int -> Map Id Int -> Map Id Type -> LPath -> Stmt -> IO [(Int, Int, Map Id Int, Map Id Type, LPath)]
    prune
      | noPrune = go
      | otherwise = prune'

    prune' :: Int -> Int -> Map Id Int -> Map Id Type -> LPath -> Stmt -> IO [(Int, Int, Map Id Int, Map Id Type, LPath)]
    prune' d _ _ _ _ _ | d < 0 = pure []
    prune' d h ids tys p s =
      checkSAT tys (conjunctiveWLP noSimplify p) >>= \case
        True -> go d h ids tys p s
        False -> pure []

    go :: Int -> Int -> Map Id Int -> Map Id Type -> LPath -> Stmt -> IO [(Int, Int, Map Id Int, Map Id Type, LPath)]
    go d _ _ _ _ _ | d < 0 = pure []
    go d h ids tys p s =
      case unFix s of
        Skip -> pure [(d, h, ids, tys, p)]
        Assume e -> pure [(d - 1, h, ids, tys, LAssume e : p)]
        Assert e -> pure [(d - 1, h, ids, tys, LAssert e : p)]
        Assign v e -> pure [(d - 1, h, ids, tys, LAssign v e : p)]
        AssignIndex v i e ->
          pure [(d - 2, h, ids, tys, LAssignIndex v i e : LAssert ((I 0 :<= i) :&& (i :< Length' v)) : p)]
        AssignNew v e ->
          pure [(d - 2, h + 1, ids, tys, LAssignIndex "H" (Var' v) e : LAssign v (I h) : p)]
        AssignVal v e ->
          pure [(d - 2, h, ids, tys, LAssignIndex "H" (Var' v) e : LAssert (Var' v :!= nullVal) : p)]
        If g t e ->
          prune (d - 1) h ids tys (LAssume g : p) t <> prune (d - 1) h ids tys (LAssume (Not' g) : p) e
        While g b -> go d h ids tys p $ If' g (Seq' b s) Skip'
        Seq a b -> go d h ids tys p a >>= fmap join . traverse (\(d, h, ids, tys, p) -> go d h ids tys p b)
        Let ds s -> go d h ids' tys' p $ substMapStmt (M.fromList $ zip names names') s
          where
            ids' = M.fromList (zip names $ (+ 1) <$> ixs) <> ids
            tys' = M.fromList (zip names' types) <> tys
            (names, types, ixs, names') = unzip4 $ ds <&> \Decl{..} ->
              let ix = M.findWithDefault 0 declName ids
              in (declName, declType, ix, "$" <> declName <> showT ix)

linearize :: Opts -> Program -> IO [(Map Id Type, LPath)]
linearize Opts{heuristics = H{..}, ..} Program{..} =
  linearizeStmt noPrune noSimplify depth programFirstPtr (programOutput : programInputs) programBody
