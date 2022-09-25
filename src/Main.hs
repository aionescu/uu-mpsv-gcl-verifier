module Main(main) where

import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Environment(getArgs)

import Language.GCL.Parser(parse)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verification(runWLP)

main :: IO ()
main = do
  [path] <- getArgs
  code <- T.readFile path

  parse path code
    >>= typeCheck
    <&> runWLP
    & either T.putStrLn print
