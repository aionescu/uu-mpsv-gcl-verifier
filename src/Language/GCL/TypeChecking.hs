module Language.GCL.TypeChecking(typeCheck) where

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

type Env = Map Id Type
type TC = ReaderT Env (Either Text)

lookupVar :: Id -> TC Type
lookupVar v = asks (M.!? v) >>= \case
  Nothing -> throwError $ "Undeclared variable " <> v
  Just ty -> pure ty

mustBe :: Type -> Type -> TC ()
mustBe expected actual =
  unless (expected == actual)
  $ throwError $ "Expected " <> showT expected <> ", found " <> showT actual

unArray :: Type -> TC Type
unArray (Array a) = pure a
unArray t = throwError $ "Expected array type, found " <> showT t

typeCheckExpr :: Type -> Expr -> TC ()
typeCheckExpr expected e = typeInferExpr e >>= mustBe expected

typeInferExpr :: Expr -> TC Type
typeInferExpr IntLit{} = pure Int
typeInferExpr BoolLit{} = pure Bool
typeInferExpr (Var v) = lookupVar v
typeInferExpr (Length v) = (lookupVar v >>= unArray) $> Int
typeInferExpr (BinOp a op b)
  | op `elem` [Add, Sub, Mul, Div] = typeCheckExpr Int a *> typeCheckExpr Int b $> Int
  | op `elem` [And, Or, Implies] = typeCheckExpr Bool a *> typeCheckExpr Bool b $> Bool
  | op `elem` [Eq, Neq, Lt, Lte, Gt, Gte] = typeCheckExpr Int a *> typeCheckExpr Int b $> Bool
  | otherwise = error "typeInferExpr: Unreachable"
typeInferExpr (Negate e) = typeInferExpr e >>= \case
  t@Array{} -> throwError $ "Expected primitive type, found " <> showT t
  t -> pure t
typeInferExpr (Subscript v e) = typeCheckExpr Int e *> lookupVar v >>= unArray
typeInferExpr (Forall v e) = withDecls [Decl v Int] $ typeCheckExpr Bool e $> Bool
typeInferExpr (Exists v e) = withDecls [Decl v Int] $ typeCheckExpr Bool e $> Bool

typeCheckDecl :: Decl -> TC Env
typeCheckDecl (Decl v t) = asks (M.!? v) >>= \case
  Just _ -> throwError $ "Cannot shadow variable " <> v
  Nothing -> pure $ M.singleton v t

withDecls :: [Decl] -> TC a -> TC a
withDecls ds m = do
  ds <- traverse typeCheckDecl ds
  local (fold ds <>) m

typeCheckStmt :: Stmt -> TC ()
typeCheckStmt Skip = pure ()
typeCheckStmt (Assume e) = void $ typeCheckExpr Bool e
typeCheckStmt (Assert e) = void $ typeCheckExpr Bool e
typeCheckStmt (Assign v e) = lookupVar v >>= (`typeCheckExpr` e)
typeCheckStmt (AssignIndex v i e) =
  typeCheckExpr Int i *> lookupVar v >>= unArray >>= (`typeCheckExpr` e)
typeCheckStmt (If g t e) = typeCheckExpr Bool g *> typeCheckStmt t *> typeCheckStmt e
typeCheckStmt (While g s) = typeCheckExpr Bool g *> typeCheckStmt s
typeCheckStmt (Seq a b) = typeCheckStmt a *> typeCheckStmt b
typeCheckStmt (Let ds s) = withDecls ds $ typeCheckStmt s

typeCheck :: Program -> Either Text Program
typeCheck p@(Program _ i o b) =
  first ("Type error: " <>)
  $ runReaderT (typeCheckStmt (Let (o : i) b) $> p) M.empty
