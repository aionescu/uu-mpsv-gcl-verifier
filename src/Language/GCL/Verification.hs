module Language.GCL.Verification(verify) where

import Control.Monad(join)
import Control.Monad.Reader(ReaderT(..), asks, local)
import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Z3.Monad hiding (local)

import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.WLP(runWLP)

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

z3Op :: BinOp -> AST -> AST -> Z AST
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
  BinOp o a b -> join $ z3Op o <$> a <*> b
  Negate a -> mkNot =<< a
  Subscript a _ -> asks (M.! a)
  Forall i e -> do
    sym <- mkStringSymbol $ T.unpack i
    sort <- mkIntSort
    var <- mkVar sym sort
    mkForall [] [sym] [sort] =<< local (M.insert i var) e
  Exists i e -> do
    sym <- mkStringSymbol $ T.unpack i
    sort <- mkIntSort
    var <- mkVar sym sort
    mkExists [] [sym] [sort] =<< local (M.insert i var) e
  Conditional g t e -> join $ mkIte <$> g <*> t <*> e

checkPred :: Map Id Type -> Pred -> Z3 Result
checkPred v p = (z3Vars v >>= runReaderT (z3Expr p) >>= mkNot >>= assert) *> check

verify :: Program -> IO ()
verify p = do
  sat <- evalZ3 $ checkPred <$> collectVars <*> runWLP $ preprocess p
  putStrLn case sat of
    Unsat -> "✔️"
    _ -> "❌"
