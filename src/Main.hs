module Main(main) where

import Control.Monad((>=>))
import Data.Function((&))
import Data.Text.IO qualified as T
import System.Environment(getArgs)

import Language.GCL.Eval(eval)
import Language.GCL.InitChecking(initCheck)
import Language.GCL.Parser(parse)
import Language.GCL.TypeChecking(typeCheck)

main :: IO ()
main = do
  (path : args) <- getArgs
  code <- T.readFile path

  parse path code
    >>= typeCheck
    >>= initCheck
    & either T.putStrLn (eval args >=> T.putStrLn)
