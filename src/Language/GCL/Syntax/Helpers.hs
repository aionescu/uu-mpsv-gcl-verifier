module Language.GCL.Syntax.Helpers where

import Data.Fix(Fix(..))

import Language.GCL.Syntax
import Data.Functor.Foldable (cata)

pattern I :: Int -> Expr
pattern I i = Fix (IntLit i)

pattern B :: Bool -> Expr
pattern B b = Fix (BoolLit b)

pattern T :: Expr
pattern T = B True

pattern F :: Expr
pattern F = B False

pattern (:&&) :: Expr -> Expr -> Expr
pattern a :&& b = Fix (Op And a b)

pattern (:||) :: Expr -> Expr -> Expr
pattern a :|| b = Fix (Op Or a b)

pattern (:=>) :: Expr -> Expr -> Expr
pattern a :=> b = Fix (Op Implies a b)

pattern (:==) :: Expr -> Expr -> Expr
pattern a :== b = Fix (Op Eq a b)

pattern (:!=) :: Expr -> Expr -> Expr
pattern a :!= b = Fix (Op Neq a b)

pattern Null' :: Expr
pattern Null' = Fix Null

pattern Not' :: Expr -> Expr
pattern Not' a = Fix (Not a)

pattern Var' :: Id -> Expr
pattern Var' a = Fix (Var a)

pattern Conditional' :: Expr -> Expr -> Expr -> Expr
pattern Conditional' g t e = Fix (Conditional g t e)

pattern Assert' :: Expr -> Stmt
pattern Assert' a = Fix (Assert a)

pattern Assume' :: Expr -> Stmt
pattern Assume' a = Fix (Assume a)

pattern If' :: Expr -> Stmt -> Stmt -> Stmt
pattern If' e s1 s2= Fix (If e s1 s2)

pattern While' :: Expr -> Stmt -> Stmt
pattern While' e s = Fix (While e s)

pattern Skip' :: Stmt
pattern Skip' = Fix Skip

pattern Seq' :: Stmt -> Stmt ->Stmt
pattern Seq' a b = Fix (Seq a b)

pattern Let' :: [Decl] -> Stmt -> Stmt
pattern Let' dc st = Fix (Let dc st)

pattern Assign' :: Id -> Expr -> Stmt
pattern Assign' i e = Fix (Assign i e)

pattern AssignIndex' :: Id -> Expr -> Expr -> Stmt
pattern AssignIndex' i e e2 = Fix (AssignIndex i e e2)

pattern AssignNew' :: Id -> Expr -> Stmt
pattern AssignNew' i e = Fix (AssignNew i e)

pattern AssignVal' :: Id -> Expr -> Stmt
pattern AssignVal' i e = Fix (AssignVal i e)

atoms :: Pred -> Int
atoms = cata \case
  Op o a b
    | o `elem` [And, Or, Implies] -> a + b
  Not b -> b
  Forall _ b -> b
  Exists _ b -> b
  Conditional g _ _ -> g
  _ -> 1
