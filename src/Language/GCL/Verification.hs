module Language.GCL.Verification(verify) where

import Control.Monad(when, unless, zipWithM_)
import Data.List(sort)
import Data.Maybe(isJust)
import Text.Printf(printf)
import System.CPUTime(getCPUTime)

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers(atoms)
import Language.GCL.Verification.Linearization(linearize)
import Language.GCL.Verification.WLP(wlp)
import Language.GCL.Verification.Z3

ratio :: Int -> Int -> String
ratio a b = printf "%d/%d (%.2f%%)" a b $ fromIntegral  a * (100 :: Double) / fromIntegral b

verify :: Opts -> Program -> IO Bool
verify Opts{..} program = do

  tStart <- getCPUTime
  let paths = linearize noHeuristics depth program
  let preds = (\(vars, p) -> (vars, p, wlp noHeuristics p)) <$> paths

  results <- traverse (\(vars, _, pred) -> checkValid vars pred) preds

  tEnd <- getCPUTime

  let
    total = length results
    invalid = length $ filter isJust results

    showResult (_, _, pred) = \case
      Nothing -> putStrLn $ "✔️  " <> show pred
      Just m -> putStrLn $ "❌ " <> show pred <> "\n" <> m

  when showStats do
    zipWithM_ showResult preds results
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
