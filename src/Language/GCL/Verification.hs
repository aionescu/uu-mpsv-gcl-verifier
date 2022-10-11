module Language.GCL.Verification(verify) where

import Data.Fix(Fix(..))
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Z3.Monad

import Language.GCL.Syntax
import Language.GCL.Verification.Preprocessing(preprocess)
import Language.GCL.Verification.WLP(runWLP)
import Data.Functor.Foldable (cata)
import Data.Functor.Foldable.Monadic (cataM)
import Data.Text (Text)

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

z3Vars :: Map Id Type -> Z3 (Map Id AST)
z3Vars = M.traverseWithKey go
  where
    go :: Id -> Type -> Z3 AST
    go i Int = mkFreshIntVar $ T.unpack i
    go i Bool = mkFreshBoolVar $ T.unpack i
    go _ (Array ty) = case ty of
      Int -> mkInteger 0
      Bool -> mkInteger 0
      _ -> error "z3Vars: Nested arrays are not supported"

z3Expr :: Map Id AST -> Expr -> Z3 AST
z3Expr vars = cataM \case
  IntLit i -> mkInteger $ toInteger i
  BoolLit b -> mkBool b
  Var v -> pure $ vars M.! v
  Length v -> pure $ vars M.! ("#" <> v)
  BinOp o a b -> case o of
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
  Negate a -> mkNot a
  -- TODO(maksymiliandemitraszek): Does GCL support bools in forall?
  Subscript a _ -> return $ vars M.! a
  Forall i e -> do 
    symb <- (mkStringSymbol.T.unpack) i
    sort <- mkIntSort
    mkForall [] [symb] [sort] e
  Exists i e -> do 
    symb <- (mkStringSymbol.T.unpack) i
    sort <- mkIntSort
    mkExists [] [symb] [sort] e
  Conditional g t e -> mkIte g t e

checkPred :: Map Id Type -> Pred -> Z3 Result
checkPred v p = (z3Vars v >>= (`z3Expr` p) >>= mkNot >>= assert) *> check

verify :: Program -> IO Text
verify p = do
  sat <- evalZ3 $ checkPred <$> collectVars <*> runWLP $ preprocess p
  case sat of
    Unsat -> return "Program is correct"
    _ -> return "Program is not correct"
--                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--                  The function type ((->) e) forms an applicative functor,
--                  so (f <$> g <*> h) is equivalent to \a -> f (g a) (h a)
