{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Verifier.Evaluator where

import Control.Monad(unless)
import Control.Monad.Except(throwError)
import Control.Monad.Reader(ReaderT, asks, local, runReaderT)
import Data.Bifunctor(first)
import Data.Foldable(fold)
import Data.Functor(($>), void)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text(Text)

import Language.GCL.Syntax
import Language.GCL.Utils(showT)

type Evaluator = Either Text
type Pred = Expr

wlp :: Stmt -> Either Text Evaluated 
wlp Skip = return (BoolLit True)
wlp (Seq h t) = do
    case h do
    (Assert g) -> BinOp And Expr   
    -- (Assume g)
    -- l <- wlp h 
    -- r <- wlp t
    -- return (BinOp l And r)

    

-- wlp (Assert g) = do

-- typeCheckStmt (Assume e) = void $ typeCheckExpr Bool 

evaluate :: Program -> Either Text Evaluated
evaluate p@(Program _ i o b) = wlp b