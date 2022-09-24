module Language.GCL.InitChecking where

import Control.Category((>>>))
import Control.Monad.Except(MonadError(throwError))
import Control.Monad.Reader(ReaderT, asks, local, runReaderT)
import Data.Bifunctor(first)
import Data.Fix(Fix(..))
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
initCheckExpr = unFix >>> \case
  IntLit{} -> pure ()
  BoolLit{} -> pure ()
  Var v -> requireInit v
  Length v -> requireInit v
  BinOp _ a b -> initCheckExpr a *> initCheckExpr b
  Negate e -> initCheckExpr e
  Subscript v e -> requireInit v *> initCheckExpr e
  Forall v e -> local (S.insert v) $ initCheckExpr e
  Exists v e -> local (S.insert v) $ initCheckExpr e

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
