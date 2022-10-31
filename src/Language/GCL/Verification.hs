module Language.GCL.Verification(verify) where

import Control.Monad(when, zipWithM_, unless)
import Data.List(sort)
import Data.Maybe(isJust)
import Text.Printf(printf)
import System.CPUTime(getCPUTime)

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Syntax.Helpers(atoms)
import Language.GCL.Verification.Linearization(linearize, removeShadowing)
import Language.GCL.Verification.WLP(wlp)
import Language.GCL.Verification.Z3

ratio :: Int -> Int -> String
ratio a b = printf "%d/%d (%.2f%%)" a b $ fromIntegral  a * (100 :: Double) / fromIntegral b

verify :: Opts -> Program -> IO Bool
verify Opts{..} program@Program{..} = do
  let
    vars = collectVars program

  tStart <- getCPUTime

  paths <- linearize noHeuristics depth program
  let preds = wlp noHeuristics <$> paths
  results <- traverse (checkValid vars) preds

  tEnd <- getCPUTime

  let
    total = length results
    invalid = length $ filter isJust results

    showResult p = \case
      Nothing -> putStrLn $ "✔️  " <> show p
      Just m -> putStrLn $ "❌ " <> show p <> "\n" <> m

  when dumpAST do
    print $ removeShadowing programBody

  when showStats do
    zipWithM_ showResult preds results

    unless noHeuristics do
      unpruned <- length <$> linearize True depth program
      putStrLn $ "Pruned paths: " <> ratio (unpruned - total) unpruned

    putStrLn $ "Invalid paths: " <> ratio invalid total

    let
      sizes = atoms <$> preds
      avg :: Double = fromIntegral (sum sizes) / fromIntegral (length sizes)
      median = sort sizes !! (length sizes `quot` 2)

    unless (null preds) do
      printf "Formula size (in atoms): average %.2f, median %d\n" avg median

    let time :: Double = fromIntegral (tEnd - tStart) / 1e12
    printf "Time elapsed: %.3fs\n" time

  pure $ invalid == 0
