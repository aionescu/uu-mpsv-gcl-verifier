module Main(main) where

import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Environment(getArgs)

import Language.GCL.Eval(eval)
import Language.GCL.Parser(parse)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verify(verify)

main :: IO ()
main = do
  (cmd : path : args) <- getArgs
  code <- T.readFile path

  let
    runCmd = case cmd of
      "run" -> print . eval args
      "wlp" -> print . verify
      _ -> error $ "Unknown command " <> show cmd

  parse path code
    >>= typeCheck
    <&> runCmd
    & either T.putStrLn id
