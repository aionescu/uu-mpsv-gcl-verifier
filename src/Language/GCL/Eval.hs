module Language.GCL.Eval(Val(..), eval) where

import Control.Applicative(liftA2, (<|>))
import Control.Monad(zipWithM)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.State(StateT, gets, modify, execStateT)
import Data.Array.IO(IOArray)
import Data.Array.MArray(MArray(..), readArray, writeArray, getElems, newListArray)
import Data.Function(on)
import Data.Functor((<&>))
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(fromJust)
import Data.Text(Text)
import Data.Text qualified as T
import Text.Read(readMaybe)

import Language.GCL.Syntax
import Language.GCL.Utils(showT)

type Array = IOArray Int

data Val
  = I { unI :: Int }
  | B { unB :: Bool }
  | A { unA :: Array Val }

instance Eq Val where
  I a == I b = a == b
  B a == B b = a == b
  _ == _ = error "(==) @Val"

instance Ord Val where
  compare (I a) (I b) = compare a b
  compare (B a) (B b) = compare a b
  compare _ _ = error "compare @Val"

instance Num Val where
  I a + I b = I $ a + b
  _ + _ = error "(+) @Val"

  I a - I b = I $ a - b
  _ - _ = error "(-) @Val"

  I a * I b = I $ a * b
  _ * _ = error "(*) @Val"

  abs (I a) = I $ abs a
  abs _ = error "abs @Val"

  signum (I a) = I $ signum a
  signum _ = error "signum @Val"

  fromInteger = I . fromInteger

showVal :: Val -> IO Text
showVal (I i) = pure $ showT i
showVal (B b) = pure $ showT b
showVal (A a) = getElems a >>= traverse showVal <&> \vs -> "[" <> T.intercalate ", " vs <> "]"

type Env = Map Id Val
type Eval = StateT Env IO

lookupVar :: Id -> Eval Val
lookupVar v = gets (M.! v)

evalOp :: BinOp -> Expr -> Expr -> Eval Val
evalOp Add = liftA2 (+) `on` evalExpr
evalOp Sub = liftA2 (-) `on` evalExpr
evalOp Mul = liftA2 (*) `on` evalExpr
evalOp Div = liftA2 ((I .) . quot `on` unI) `on` evalExpr
evalOp Eq = liftA2 ((B .) . (==)) `on` evalExpr
evalOp Neq = liftA2 ((B .) . (/=)) `on` evalExpr
evalOp Lt = liftA2 ((B .) . (<)) `on` evalExpr
evalOp Lte = liftA2 ((B .) . (<=)) `on` evalExpr
evalOp Gt = liftA2 ((B .) . (>)) `on` evalExpr
evalOp Gte = liftA2 ((B .) . (>=)) `on` evalExpr
evalOp And = \a b -> evalExpr a >>= \case
  B True -> evalExpr b
  a -> pure a
evalOp Or = \a b -> evalExpr a >>= \case
  B False -> evalExpr b
  a -> pure a
evalOp Implies = \a b -> evalExpr a >>= \case
  B False -> pure $ B True
  _ -> evalExpr b

evalExpr :: Expr -> Eval Val
evalExpr (IntLit i) = pure $ I i
evalExpr (BoolLit b) = pure $ B b
evalExpr (Var v) = lookupVar v
evalExpr (Length v) = liftIO . fmap (I . (+ 1) . snd) . getBounds . unA =<< lookupVar v
evalExpr (BinOp a o b) = evalOp o a b
evalExpr (Negate e) = evalExpr e >>= \case
  I i -> pure $ I $ negate i
  B b -> pure $ B $ not b
  _ -> error "evalExpr: Negate"
evalExpr (Subscript v e) = do
  a <- unA <$> lookupVar v
  i <- unI <$> evalExpr e
  liftIO $ readArray a i
evalExpr (Forall v e) = B . all unB <$> traverse (instantiate v e) [-100 .. 100]
evalExpr (Exists v e) = B . any unB <$> traverse (instantiate v e) [-100 .. 100]

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
  liftIO $ writeArray a i e
evalStmt (If g t e) = evalExpr g >>= \case
  B True -> evalStmt t
  _ -> evalStmt e
evalStmt w@(While g s) = evalExpr g >>= \case
  B True -> evalStmt s *> evalStmt w
  _ -> pure ()
evalStmt (Seq a b) = evalStmt a *> evalStmt b
evalStmt (Let ds s) = evalStmt s *> modify \m -> foldr M.delete m $ declName <$> ds

readVal :: String -> IO Val
readVal s =
  fromJust
  $ pure . I <$> readMaybe @Int s
  <|> pure . B <$> readMaybe @Bool s
  <|> arr I <$> readMaybe @[Int] s
  <|> arr B <$> readMaybe @[Bool] s
  where
    arr f l = A <$> newListArray (0, length l - 1) (f <$> l)

getInputs :: [Decl] -> [String] -> IO Env
getInputs ds vs
  | length ds /= length vs = error "Invalid number of arguments"
  | otherwise = M.fromList <$> zipWithM (\(Decl v _) s -> (v,) <$> readVal s) ds vs

eval :: [String] -> Program -> IO Text
eval args (Program _ i o@(Decl v _) b) =
  showVal . (M.! v) =<< execStateT (evalStmt b) =<< getInputs (i <> [o]) args
