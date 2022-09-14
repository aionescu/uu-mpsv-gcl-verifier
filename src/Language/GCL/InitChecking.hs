module Language.GCL.InitChecking where

import Control.Monad.Except(MonadError(throwError))
import Control.Monad.Reader(ReaderT, asks, local, runReaderT)
import Data.Bifunctor(first)
import Data.Functor(($>))
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)

import Language.GCL.Syntax

type Env = Set Id
type Init = ReaderT Env (Either Text)

requireInit :: Id -> Init ()
requireInit v = asks (S.member v) >>= \case
  False -> throwError $ "Uninitialized variable " <> v
  _ -> pure ()

initCheckExpr :: Expr -> Init ()
initCheckExpr IntLit{} = pure ()
initCheckExpr BoolLit{} = pure ()
initCheckExpr (Var v) = requireInit v
initCheckExpr (Length v) = requireInit v
initCheckExpr (BinOp a _ b) = initCheckExpr a *> initCheckExpr b
initCheckExpr (Negate e) = initCheckExpr e
initCheckExpr (Subscript v e) = requireInit v *> initCheckExpr e
initCheckExpr (Forall v e) = local (S.insert v) $ initCheckExpr e
initCheckExpr (Exists v e) = local (S.insert v) $ initCheckExpr e

initCheckStmt :: Stmt -> Init Env
initCheckStmt Skip = pure S.empty
initCheckStmt (Assume e) = initCheckExpr e $> S.empty
initCheckStmt (Assert e) = initCheckExpr e $> S.empty
initCheckStmt (Assign v e) = initCheckExpr e $> S.singleton v
initCheckStmt (AssignIndex v i e) = requireInit v *> initCheckExpr i *> initCheckExpr e $> S.empty
initCheckStmt (If g t e) = initCheckExpr g *> (S.intersection <$> initCheckStmt t <*> initCheckStmt e)
initCheckStmt (While g s) = initCheckExpr g *> initCheckStmt s $> S.empty
initCheckStmt (Seq a b) = initCheckStmt a >>= \s -> local (s <>) $ initCheckStmt b
initCheckStmt (Let ds s) = (S.\\ vs) <$> initCheckStmt s
  where
    vs = S.fromList $ declName <$> ds

initCheck :: Program -> Either Text Program
initCheck p@(Program _ i o b) =
  first ("Init error: " <>)
  $ runReaderT (initCheckStmt b $> p)
  $ S.fromList $ declName <$> o : i
