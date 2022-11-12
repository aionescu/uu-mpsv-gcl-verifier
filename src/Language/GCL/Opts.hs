module Language.GCL.Opts where

import Options.Applicative

data Opts =
  Opts
  { depth :: Int
  , _N :: Maybe Int
  , noHeuristics :: Bool
  , dumpAST :: Bool
  , showStats :: Bool
  , showPaths :: Bool
  , showPreds :: Bool
  , path :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> option auto (long "depth" <> value 50 <> metavar "K" <> help "Maximum depth of program paths (default 50)")
  <*> optional (option auto $ long "N" <> metavar "N" <> help "Experiment variable (optional)")
  <*> switch (long "no-heuristics" <> help "Disable feasiblity heuristics & frontend simplifier")
  <*> switch (long "dump-ast" <> help "Show AST after parsing")
  <*> switch (long "show-stats" <> help "Show verification statistics")
  <*> switch (long "show-paths" <> help "Show paths and results")
  <*> switch (long "show-preds" <> help "Show preds and results")
  <*> strArgument (metavar "PATH" <> help "The source file to verify")

fullParser :: ParserInfo Opts
fullParser = info (helper <*> optsParser) (fullDesc <> header "GCL Program Verifier")

getOpts :: IO Opts
getOpts = execParser fullParser
