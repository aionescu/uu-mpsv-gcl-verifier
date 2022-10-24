module Language.GCL.Verification(verify) where

import Control.Monad(join, when, zipWithM_, unless)
import Control.Monad.Reader(ReaderT(..), asks, local)
import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Z3.Monad hiding (Opts, local, simplify)

import Language.GCL.Opts
import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.WLP(wlp)
import Data.Maybe (isJust)
import Data.Functor ((<&>))
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Language.GCL.Syntax.Helpers (atoms)
import Data.List (sort)

type Env = Map Id AST
type Z = ReaderT Env Z3

collectVars :: Program -> Map Id Type
collectVars Program{..} = go $ Fix $ Let (programOutput : programInputs) programBody
  where
    decls :: [Decl] -> Map Id Type
    decls ds = M.fromList $ ds >>= \case
      Decl i (Array t) -> [(i, t), ("#" <> i, Int)]
      Decl i t -> [(i, t)]

    go :: Stmt -> Map Id Type
    go = cata \case
      If _ t e -> t <> e
      While _ s -> s
      Seq a b -> a <> b
      Let ds s -> decls ds <> s
      _ -> M.empty

z3Vars :: Map Id Type -> Z3 Env
z3Vars = M.traverseWithKey go
  where
    go :: Id -> Type -> Z3 AST
    go i Int = mkFreshIntVar $ T.unpack i
    go i Bool = mkFreshBoolVar $ T.unpack i
    go _ (Array ty) = case ty of
      Int -> mkInteger 0
      Bool -> mkBool False
      _ -> error "z3Vars: Nested arrays are not supported"

z3Op :: Op -> AST -> AST -> Z AST
z3Op op a b =
  case op of
    Add -> mkAdd [a, b]
    Sub -> mkSub [a, b]
    Mul -> mkMul [a, b]
    Div -> mkDiv a b
    And -> mkAnd [a, b]
    Or -> mkOr [a, b]
    Implies -> mkImplies a b
    Eq -> mkEq a b
    Neq -> mkNot =<< mkEq a b
    Lt -> mkLt a b
    Lte -> mkLe a b
    Gt -> mkGt a b
    Gte -> mkGe a b

z3Expr :: Expr -> Z AST
z3Expr = cata \case
  IntLit i -> mkInteger $ toInteger i
  BoolLit b -> mkBool b
  Var v -> asks (M.! v)
  Length v -> asks (M.! ("#" <> v))
  Op o a b -> join $ z3Op o <$> a <*> b
  Negate a -> mkUnaryMinus =<< a
  Not a -> mkNot =<< a
  Subscript a _ -> asks (M.! a)
  Forall i e -> do
    var <- mkFreshIntVar $ T.unpack i
    app <- toApp var
    mkForallConst [] [app] =<< local (M.insert i var) e
  Exists i e -> do
    var <- mkFreshIntVar $ T.unpack i
    app <- toApp var
    mkExistsConst [] [app] =<< local (M.insert i var) e
  Conditional g t e -> join $ mkIte <$> g <*> t <*> e

checkValid :: Map Id Type -> Pred -> Z3 (Maybe String)
checkValid v p = (z3Vars v >>= runReaderT (z3Expr p) >>= mkNot >>= assert) *> withModel modelToString <&> snd

ratio :: Int -> Int -> String
ratio a b = printf "%d/%d (%.2f%%)" a b $ fromIntegral  a * (100 :: Double) / fromIntegral b

verify :: Opts -> Program -> IO Bool
verify Opts{..} program = do
  let
    p = preprocess program
    vars = collectVars p
    preds = wlp noSimplify depth p

  tStart <- getCPUTime
  results <- traverse (evalZ3 . checkValid vars) preds
  tEnd <- getCPUTime

  let
    total = length results
    invalid = length $ filter isJust results

    showResult p = \case
      Nothing -> putStrLn $ "✔️  " <> show p
      Just m -> putStrLn $ "❌ " <> show p <> "\n" <> m

  when dumpAST do
    print p

  when showStats do
    zipWithM_ showResult preds results

    unless noSimplify do
      let unpruned = length $ wlp True depth p
      putStrLn $ "Pruned paths: " <> ratio (unpruned - total) unpruned

    putStrLn $ "Invalid paths: " <> ratio invalid total

    let
      sizes = atoms <$> preds
      avg :: Double = fromIntegral (sum sizes) / fromIntegral (length sizes)
      median = sort sizes !! (length sizes `quot` 2)

    unless (null preds) do
      printf "Formula size (in atoms): average %.2f, median %d\n" avg median

    let time :: Double = fromIntegral (tEnd - tStart) / 1e12
    printf "Time elapsed: %.3fs\n" time

  pure $ invalid == 0
