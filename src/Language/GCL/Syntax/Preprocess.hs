module Language.GCL.Syntax.Preprocess(preprocess) where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)

import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers

removeN :: Maybe Int -> Program -> Program
removeN (Just (I -> n)) p@Program{..} | Decl "N" Int `elem` programInputs =
  p
  { programInputs = filter (/= Decl "N" Int) programInputs
  , programBody = mapExprs (cata \case Var "N" -> n; e -> Fix e) programBody
  }
removeN _ p = p

rewritePtrs :: Program -> Program
rewritePtrs p@Program{..} = p { programBody = mapExprs go programBody }
  where
    go = cata \case
      GetVal v -> Subscript' (Var' "H") $ Var' v
      Null -> nullVal
      e -> Fix e

addDistinctPre :: Program -> Program
addDistinctPre p@Program{..} =
  p { programBody = foldr addPre programBody refs, programFirstPtr = refCount }
  where
    refs = filter ((== Ref) . declType) $ programOutput : programInputs
    refCount = length refs

    addPre (Decl n _) = Seq' $ Assume' $ (I 0 :<= Var' n) :&& (Var' n :< I (length refs))

preprocess :: Maybe Int -> Program -> Program
preprocess n = addDistinctPre . rewritePtrs . removeN n
