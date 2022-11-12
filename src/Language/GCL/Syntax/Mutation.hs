module Language.GCL.Syntax.Mutation where

import Control.Monad((<=<))
import Data.Bool(bool)
import Data.Functor.Foldable(para)
import Data.Map.Strict(Map)
import System.CPUTime(getCPUTime)
import Text.Printf(printf)

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Utils
import Language.GCL.Verification.Linearization
import Language.GCL.Verification.WLP
import Language.GCL.Verification.Z3

mutateOp :: Op -> Expr -> Expr -> [Expr]
mutateOp o a b =
  case o of
    Add -> [a, b, a :- b]
    Sub -> [a, a :+ b, b :- a]
    Mul -> [a, b]
    Div -> [a, b :/ a]
    And -> [a, b, a :|| b]
    Or -> [a, b, a :&& b]
    Implies -> [a, b, b :=> a]
    Eq -> [a :!= b]
    Neq -> [a :== b]
    Lt -> [a :<= b, a :> b]
    Lte -> [a :< b, a :>= b]
    Gt -> [a :< b, a :>= b]
    Gte -> [a :<= b, a :> b]

mutateExpr :: Expr -> [Expr]
mutateExpr = para \case
  IntLit i -> [I $ i - 1, I $ i + 1]
  BoolLit b -> [B $ not b]
  Null -> []
  Var{} -> []
  GetVal{} -> []
  Length{} -> []
  Op o (a, ma) (b, mb) -> mutateOp o a b <> ((\a -> Op' o a b) <$> ma) <> (Op' o a <$> mb)
  Negate (e, me) -> e : me
  Not (e, me) -> e : me
  Subscript (a, ma) (i, mi) -> ((`Subscript'` i) <$> ma) <> (Subscript' a <$> mi)
  Forall{} -> []
  Exists{} -> []
  RepBy{} -> []

mutateStmt :: Stmt -> [Stmt]
mutateStmt = para \case
  Skip -> []
  Assume{} -> []
  Assert{} -> []
  Assign v e -> Assign' v <$> mutateExpr e
  AssignIndex v i e -> (AssignIndex' v i <$> mutateExpr e) <> ((\i -> AssignIndex' v i e) <$> mutateExpr i)
  AssignNew v e -> AssignNew' v <$> mutateExpr e
  AssignVal v e -> AssignVal' v <$> mutateExpr e
  If g (t, mt) (e, me) ->
    ((\g -> If' g t e) <$> mutateExpr g) <> ((\t -> If' g t e) <$> mt) <> (If' g t <$> me)
    <> case e of Skip' -> []; _ -> [If' g t Skip']
  While g (b, mb) -> ((`While'` b) <$> mutateExpr g) <> (While' g <$> mb)
  Seq (a, ma) (b, mb) -> ((`Seq'` b) <$> ma) <> (Seq' a <$> mb)
  Let ds (_, ms) -> Let' ds <$> ms

anyPathKilled :: Bool -> [(Map Id Type, LPath)] -> IO Bool
anyPathKilled _ [] = pure False
anyPathKilled noHeuristics ((tys, p) : ps) =
  checkValid tys (wlp noHeuristics p) >>= \case
    Just{} -> pure True
    Nothing -> anyPathKilled noHeuristics ps

checkMutations :: Opts -> Program -> IO Bool
checkMutations Opts{heuristics = H{..}, ..} Program{..} = do
  let
    decls = programOutput : programInputs
    mutations = mutateStmt programBody
    checkKilled =
      anyPathKilled noSimplify
      <=< linearizeStmt noPrune noSimplify depth programFirstPtr decls

  tStart <- getCPUTime
  results <- traverse checkKilled mutations
  tEnd <- getCPUTime

  let
    total = length results
    killed = length $ filter id results

  putStrLn ""
  putStrLn $ "Inputs: " <> path <> ", depth = " <> show depth <> ", N = " <> show _N <> bool "" ", noPrune" noPrune <> bool "" ", noSimplify" noSimplify
  putStrLn $ "Mutants killed: " <> ratio killed total

  let time :: Double = fromIntegral (tEnd - tStart) / 1e12
  printf "Time elapsed: %.3fs\n" time

  pure $ killed > 0
