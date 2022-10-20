module Main(main) where

import Data.Bool(bool)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Exit(ExitCode(..), exitWith)

import Language.GCL.Opts
import Language.GCL.Parser(parse)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verification(verify)

main :: IO ()
main = do
  opts@Opts{..} <- getOpts
  code <- T.readFile path

  code
    & parse opts
    >>= typeCheck
    <&> verify opts
    & either ((False <$) . T.putStrLn) id
    >>= exitWith . bool (ExitFailure 1) ExitSuccess
