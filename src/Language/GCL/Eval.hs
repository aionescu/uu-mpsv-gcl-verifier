module Language.GCL.Eval(Val(..), eval) where

import Control.Applicative(liftA2, (<|>))
import Control.Monad.State.Strict(State, evalState, gets, modify)
import Data.Function(on)
import Data.Functor.Foldable(cata)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Set(Set)
import Data.Set qualified as S
import Text.Read(Read(..), ReadPrec)

import Language.GCL.Syntax
import Language.GCL.Utils((...))

data Val
  = I { unI :: Int }
  | B { unB :: Bool }
  | A { unA :: Vector Val }
  deriving (Eq, Ord)

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (A a) = show a

instance Read Val where
  readPrec :: ReadPrec Val
  readPrec =
    I <$> readPrec @Int
    <|> B <$> readPrec @Bool
    <|> A <$> readPrec @(Vector Val)

type Env = Map Id Val
type Eval = State Env

lookupVar :: Id -> Eval Val
lookupVar v = gets (M.! v)

shadow :: Set Id -> Eval a -> Eval a
shadow vs m = do
  env <- gets (`M.restrictKeys` vs)
  a <- m
  modify \m -> env <> M.withoutKeys m vs
  pure a

evalOp :: BinOp -> Eval Val -> Eval Val -> Eval Val
evalOp Add = liftA2 (I ... (+) `on` unI)
evalOp Sub = liftA2 (I ... (-) `on` unI)
evalOp Mul = liftA2 (I ... (*) `on` unI)
evalOp Div = liftA2 (I ... quot `on` unI)
evalOp Eq = liftA2 $ B ... (==)
evalOp Neq = liftA2 $ B ... (/=)
evalOp Lt = liftA2 $ B ... (<)
evalOp Lte = liftA2 $ B ... (<=)
evalOp Gt = liftA2 $ B ... (>)
evalOp Gte = liftA2 $ B ... (>=)
evalOp And = \a b -> a >>= \case
  B True -> b
  a -> pure a
evalOp Or = \a b -> a >>= \case
  B False -> b
  a -> pure a
evalOp Implies = \a b -> a >>= \case
  B False -> pure $ B True
  _ -> b

instantiate :: Id -> Eval Val -> Int -> Eval Val
instantiate v e i = modify (M.insert v $ I i) *> e

evalExpr :: Expr -> Eval Val
evalExpr = cata \case
  IntLit i -> pure $ I i
  BoolLit b -> pure $ B b
  Var v -> lookupVar v
  Length v -> I . V.length . unA <$> lookupVar v
  BinOp o a b -> evalOp o a b
  Negate e -> e >>= \case
    I i -> pure $ I $ negate i
    B b -> pure $ B $ not b
    _ -> error "evalExpr: Unreachable"
  Subscript v e -> (V.!) <$> (unA <$> lookupVar v) <*> (unI <$> e)
  Forall v e -> shadow (S.singleton v) $ B . all unB <$> traverse (instantiate v e) [-100 .. 100]
  Exists v e -> shadow (S.singleton v) $ B . any unB <$> traverse (instantiate v e) [-100 .. 100]

evalStmt :: Stmt -> Eval ()
evalStmt Skip = pure ()
evalStmt (Assume e) = evalExpr e >>= \case
  B False -> error "assume failed"
  _ -> pure ()
evalStmt (Assert e) = evalExpr e >>= \case
  B False -> error "assert failed"
  _ -> pure ()
evalStmt (Assign v e) = modify . M.insert v =<< evalExpr e
evalStmt (AssignIndex v i e) = do
  a <- unA <$> lookupVar v
  i <- unI <$> evalExpr i
  e <- evalExpr e
  modify (M.insert v $ A $ a V.// [(i, e)])
evalStmt (If g t e) = evalExpr g >>= \case
  B True -> evalStmt t
  _ -> evalStmt e
evalStmt w@(While g s) = evalExpr g >>= \case
  B True -> evalStmt s *> evalStmt w
  _ -> pure ()
evalStmt (Seq a b) = evalStmt a *> evalStmt b
evalStmt (Let ds s) = shadow (S.fromList $ declName <$> ds) $ evalStmt s

getInputs :: [Decl] -> [String] -> Env
getInputs ds vs
  | length ds /= length vs = error "Invalid number of arguments"
  | otherwise = M.fromList $ zipWith (\(Decl v _) s -> (v, read s)) ds vs

eval :: [String] -> Program -> Val
eval args (Program _ i d@(Decl v _) b) =
  evalState (evalStmt b *> lookupVar v) $ getInputs (i <> [d]) args
