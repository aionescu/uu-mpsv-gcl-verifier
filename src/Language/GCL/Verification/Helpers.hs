module Language.GCL.Verification.Helpers where

import Data.Fix(Fix(..))
import Data.Functor.Foldable(cata, para)

import Language.GCL.Syntax
import Language.GCL.Utils((...))

pattern I :: Int -> Expr
pattern I i = Fix (IntLit i)

pattern B :: Bool -> Expr
pattern B b = Fix (BoolLit b)

pattern T :: Expr
pattern T = B True

pattern F :: Expr
pattern F = B False

(∧) :: Expr -> Expr -> Expr
(∧) = Fix ... BinOp And

(∨) :: Expr -> Expr -> Expr
(∨) = Fix ... BinOp Or

(⟹) :: Expr -> Expr -> Expr
(⟹) = Fix ... BinOp Implies

(¬) :: Expr -> Expr
(¬) = Fix . Negate