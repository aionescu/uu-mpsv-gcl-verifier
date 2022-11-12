module Language.GCL.Verification(verify) where

import Control.Monad(when, unless)
import Data.List(sort)
import Data.Maybe(isJust)
import Text.Printf(printf)
import System.CPUTime(getCPUTime)

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers
import Language.GCL.Verification.Linearization(linearize)
import Language.GCL.Verification.WLP(wlp)
import Language.GCL.Verification.Z3
import Language.GCL.Utils(ratio)


addDistinctPre :: Program -> (LPath, Int)
addDistinctPre Program{..} = (map (\Decl{..} -> LAssume ((I 0 :>= Var' declName) :&& (Var' declName :< I (length refs)))) refs, length refs)
  where refs = filter (\Decl{..} -> declType == Ref) (programOutput : programInputs)

verify :: Opts -> Program -> IO Bool
verify opts@Opts{..} program = do
  tStart <- getCPUTime
  let (precond, h) = addDistinctPre program
  paths <- linearize opts program h
  let paths' = map (\(m, p) -> (m, p <> precond)) paths
  let preds = (\(vars, p) -> (vars, p, wlp noSimplify p)) <$> paths'

  results <- traverse (\(vars, _, pred) -> checkValid vars pred) preds
  tEnd <- getCPUTime

  let
    total = length results
    invalid = length $ filter isJust results

    showResult ((_, p, pr), result) = do
      when showPaths $ print $ reverse p
      when showPreds $ print pr
      case result of
        Nothing -> putStrLn "✔️  "
        Just m -> putStrLn $ "❌ " <> "\n" <> m

  when (showPaths || showPreds) do
    mapM_ showResult $ zip preds results

  when showStats do
    putStrLn $ "Invalid paths: " <> ratio invalid total

    let
      sizes = (\(_, _, pred) -> atoms pred) <$> preds
      avg :: Double = fromIntegral (sum sizes) / fromIntegral (length sizes)
      median = sort sizes !! (length sizes `quot` 2)

    unless (null preds) do
      printf "Formula size (in atoms): average %.2f, median %d\n" avg median

    let time :: Double = fromIntegral (tEnd - tStart) / 1e12
    printf "Time elapsed: %.3fs\n" time

  pure $ invalid == 0
