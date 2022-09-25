module Language.GCL.TypeChecking(typeCheck) where

import Control.Monad.Except(throwError)
import Control.Monad.Reader(ReaderT, asks, local, runReaderT)
import Data.Bifunctor(first)
import Data.Functor(($>), (<&>), void)
import Data.Functor.Foldable(cata)
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

unArray :: Type -> TC Type
unArray (Array a) = pure a
unArray t = throwError $ "Expected array type, found " <> showT t

unPrim :: Type -> TC Type
unPrim t@Array{} = throwError $ "Expected primitive type, found " <> showT t
unPrim t = pure t

check :: Type -> TC Type -> TC Type
check expected m = m >>= \t ->
  if t == expected
  then pure t
  else throwError $ "Expected " <> showT expected <> ", found " <> showT t

typeInferExpr :: Expr -> TC Type
typeInferExpr = cata \case
  IntLit{} -> pure Int
  BoolLit{} -> pure Bool
  Var v -> lookupVar v
  Length v -> (lookupVar v >>= unArray) $> Int
  BinOp op a b
    | op `elem` [Add, Sub, Mul, Div] -> check Int a *> check Int b $> Int
    | op `elem` [And, Or, Implies] -> check Bool a *> check Bool b $> Bool
    | op `elem` [Eq, Neq, Lt, Lte, Gt, Gte] -> check Int a *> check Int b $> Bool
    | otherwise -> error "typeInferExpr: Unreachable"
  Negate e -> e >>= unPrim
  Subscript v e -> check Int e *> lookupVar v >>= unArray
  Forall v e -> with [Decl v Int] $ check Bool e $> Bool
  Exists v e -> with [Decl v Int] $ check Bool e $> Bool

typeCheckExpr :: Type -> Expr -> TC ()
typeCheckExpr t e = void $ check t $ typeInferExpr e

with :: [Decl] -> TC a -> TC a
with ds m
  | M.size env /= length ds = throwError "Cannot shadow within the same binding"
  | otherwise = local (env <>) m
  where
    env = M.fromList $ ds <&> \(Decl v t) -> (v, t)

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
typeCheckStmt (Let ds s) = with ds $ typeCheckStmt s

typeCheck :: Program -> Either Text Program
typeCheck p@(Program _ i o b) =
  first ("Type error: " <>)
  $ runReaderT (typeCheckStmt (Let (o : i) b) $> p) M.empty
