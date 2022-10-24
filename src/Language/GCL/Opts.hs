module Language.GCL.Opts where

import Options.Applicative

data Opts =
  Opts
  { depth :: Int
  , noSimplify :: Bool
  , showStats :: Bool
  , dumpAST :: Bool
  , path :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> option auto (long "depth" <> value 50 <> metavar "K" <> help "Maximum depth of program paths (default 50)")
  <*> switch (long "no-simplify" <> help "Disable frontend simplifier")
  <*> switch (long "show-stats" <> help "Show verification statistics")
  <*> switch (long "dump-ast" <> help "Show AST after pre-processing")
  <*> strArgument (metavar "PATH" <> help "The source file to verify")

fullParser :: ParserInfo Opts
fullParser = info (helper <*> optsParser) (fullDesc <> header "GCL Program Verifier")

getOpts :: IO Opts
getOpts = execParser fullParser
