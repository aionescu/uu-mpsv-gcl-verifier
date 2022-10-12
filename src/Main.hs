module Main(main) where

import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Environment(getArgs)

import Language.GCL.Parser(parse)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verification(verify)

main :: IO ()
main = do
  [path] <- getArgs
  code <- T.readFile path

  parse path code
    >>= typeCheck
    <&> verify
    & either T.putStrLn (>>= T.putStrLn)

