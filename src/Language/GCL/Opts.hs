module Language.GCL.Opts where

import Options.Applicative

data Opts =
  Opts
  { unrollDepth :: Int
  , noSimplify :: Bool
  , showStats :: Bool
  , path :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> option auto (long "unroll-depth" <> value 10 <> metavar "K" <> help "How many iterations to unroll for each loop")
  <*> switch (long "no-simplify" <> help "Disable frontend simplifier")
  <*> switch (long "show-stats" <> help "Show verification statistics")
  <*> strArgument (metavar "PATH" <> help "The source file to verify")

fullParser :: ParserInfo Opts
fullParser = info (helper <*> optsParser) (fullDesc <> header "GCL Program Verifier")

getOpts :: IO Opts
getOpts = execParser fullParser
