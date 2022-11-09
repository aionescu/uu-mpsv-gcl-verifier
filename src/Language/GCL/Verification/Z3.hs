module Language.GCL.Verification.Z3 where

import Control.Monad(join)
import Control.Monad.Reader(ReaderT(..), asks, local)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Functor.Foldable(cata)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Z3.Monad hiding(Opts, local, simplify)

import Language.GCL.Syntax

type Env = Map Id AST
type Z = ReaderT Env Z3

collectVars :: Program -> Map Id Type
collectVars Program{..} = decls (programOutput : programInputs)
  where
    decls :: [Decl] -> Map Id Type
    decls ds = M.fromList $ ds >>= \case
      Decl i (Array t) -> [(i, t), ("#" <> i, Int)]
      Decl i t -> [(i, t)]

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

checkValid :: Map Id Type -> Pred -> IO (Maybe String)
checkValid v p =
  (z3Vars v >>= runReaderT (z3Expr p) >>= mkNot >>= assert)
  *> withModel modelToString
  <&> snd
  & evalZ3

checkSAT :: Map Id Type -> Pred -> IO Bool
checkSAT v p =
  (z3Vars v >>= runReaderT (z3Expr p) >>= assert)
  *> check
  <&> (== Sat)
  & evalZ3