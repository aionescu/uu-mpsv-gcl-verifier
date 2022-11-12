module Language.GCL.Opts where

import Options.Applicative

data Opts =
  Opts
  { depth :: Int
  , _N :: Maybe Int
  , noPrune :: Bool
  , noSimplify :: Bool
  , dumpAST :: Bool
  , showStats :: Bool
  , showPaths :: Bool
  , showPreds :: Bool
  , mutate :: Bool
  , path :: FilePath
  }

data Heuristics = Heuristics { noPrune:: Bool , noSimplify:: Bool }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> option auto (long "depth" <> value 50 <> metavar "K" <> help "Maximum depth of program paths (default 50)")
  <*> optional (option auto $ long "N" <> metavar "N" <> help "")
  <*> switch (long "no-prune" <> help "Disable pruining unfeasible paths")
  <*> switch (long "no-simplify" <> help "Disable front-end simplifier")
  <*> switch (long "dump-ast" <> help "Show AST after parsing")
  <*> switch (long "show-stats" <> help "Show verification statistics")
  <*> switch (long "show-paths" <> help "Show paths and results")
  <*> switch (long "show-preds" <> help "Show preds and results")
  <*> switch (long "mutate" <> help "Run with mutations")
  <*> strArgument (metavar "PATH" <> help "The source file to verify")

fullParser :: ParserInfo Opts
fullParser = info (helper <*> optsParser) (fullDesc <> header "GCL Program Verifier")

getOpts :: IO Opts
getOpts = execParser fullParser
