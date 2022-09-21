module Main(main) where

import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Environment(getArgs)

import Language.GCL.InitChecking(initCheck)
import Language.GCL.Parser(parse)
import Language.GCL.PrettyPrint(pPrint)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verification(runWLP)

main :: IO ()
main = do
  [path] <- getArgs
  code <- T.readFile path

  parse path code
    >>= typeCheck
    >>= initCheck
    <&> runWLP
    & either T.putStrLn pPrint
