module Main(main) where

import Data.Bool(bool)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Text.IO qualified as T
import System.Exit(ExitCode(..), exitWith)

import Language.GCL.Opts
import Language.GCL.Parser(parse)
import Language.GCL.Syntax(Program)
import Language.GCL.Syntax.Mutation(checkMutations)
import Language.GCL.Syntax.Preprocess(preprocess)
import Language.GCL.TypeChecking(typeCheck)
import Language.GCL.Verification(verify)

run :: Opts -> Program -> IO Bool
run opts@Opts{..}
  | dumpAST = (True <$) . print
  | mutate = flip (checkMutations opts) 10
  | otherwise = verify opts

main :: IO ()
main = do
  opts@Opts{..} <- getOpts
  code <- T.readFile path

  code
    & parse opts
    >>= typeCheck
    <&> preprocess _N
    <&> run opts
    & either ((False <$) . T.putStrLn) id
    >>= exitWith . bool (ExitFailure 1) ExitSuccess
