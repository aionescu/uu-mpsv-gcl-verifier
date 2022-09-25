module Language.GCL.Eval(Val(..), eval) where

import Control.Category((>>>))
import Control.Monad.State.Strict(State, evalState, gets, modify)
import Data.Fix(Fix(..))
import Data.Function(on)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(fromJust)
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Set(Set)
import Data.Set qualified as S
import Text.Read(readMaybe)

import Language.GCL.Syntax

data Val
  = I { unI :: Int }
  | B { unB :: Bool }
  | A { unA :: Vector Val }
  deriving (Eq, Ord)

instance Show Val where
  show (I i) = show i
  show (B b) = show b
  show (A a) = show a

instance Num Val where
  (+) = (I .) . (+) `on` unI
  (-) = (I .) . (-) `on` unI
  (*) = (I .) . (*) `on` unI

  abs = I . abs . unI
  signum = I . signum . unI
  fromInteger = I . fromInteger

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

evalOp :: BinOp -> Expr -> Expr -> Eval Val
evalOp And a b = evalExpr a >>= \case
  B True -> evalExpr b
  a -> pure a
evalOp Or a b = evalExpr a >>= \case
  B False -> evalExpr b
  a -> pure a
evalOp Implies a b = evalExpr a >>= \case
  B False -> pure $ B True
  _ -> evalExpr b
evalOp o a b = op o <$> evalExpr a <*> evalExpr b
  where
    op Add = (+)
    op Sub = (-)
    op Mul = (*)
    op Div = (I .) . quot `on` unI
    op Eq = (B .) . (==)
    op Neq = (B .) . (/=)
    op Lt = (B .) . (<)
    op Lte = (B .) . (<=)
    op Gt = (B .) . (>)
    op Gte = (B .) . (>=)
    op _ = error "op: Unreachable"

evalExpr :: Expr -> Eval Val
evalExpr = unFix >>> \case
  IntLit i -> pure $ I i
  BoolLit b -> pure $ B b
  Var v -> lookupVar v
  Length v -> I . V.length . unA <$> lookupVar v
  BinOp o a b -> evalOp o a b
  Negate e -> evalExpr e >>= \case
    I i -> pure $ I $ negate i
    B b -> pure $ B $ not b
    _ -> error "evalExpr: Unreachable"
  Subscript v e -> (V.!) <$> (unA <$> lookupVar v) <*> (unI <$> evalExpr e)
  Forall v e -> shadow (S.singleton v) $ B . all unB <$> traverse (instantiate v e) [-100 .. 100]
  Exists v e -> shadow (S.singleton v) $ B . any unB <$> traverse (instantiate v e) [-100 .. 100]

instantiate :: Id -> Expr -> Int -> Eval Val
instantiate v e i = modify (M.insert v $ I i) *> evalExpr e

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

readVal :: Type -> String -> Maybe Val
readVal Int s = I <$> readMaybe @Int s
readVal Bool s = B <$> readMaybe @Bool s
readVal (Array Int) s = A . fmap I <$> readMaybe @(Vector Int) s
readVal (Array Bool) s = A . fmap B <$> readMaybe @(Vector Bool) s
readVal _ _ = error "readVal: Ill-formed type"

getInputs :: [Decl] -> [String] -> Env
getInputs ds vs
  | length ds /= length vs = error "Invalid number of arguments"
  | otherwise = M.fromList $ zipWith (\(Decl v t) s -> (v, fromJust $ readVal t s)) ds vs

eval :: [String] -> Program -> Val
eval args (Program _ i d@(Decl v _) b) =
  evalState (evalStmt b *> lookupVar v) $ getInputs (i <> [d]) args
