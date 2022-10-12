module Language.GCL.Syntax.Helpers where

import Data.Fix(Fix(..))

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

ifEqExpr :: Expr -> Expr -> Expr -> Expr -> Expr
ifEqExpr a b e1 e2 = Fix $ Conditional (Fix $ BinOp Eq a b) e1 e2


assertSt :: Expr -> Stmt
assertSt = Fix . Assert

assumeSt :: Expr -> Stmt
assumeSt = Fix . Assume


ifSt :: Expr -> Stmt -> Stmt -> Stmt
ifSt e s1 s2= Fix $ If e s1 s2

whileSt :: Expr -> Stmt -> Stmt
whileSt e s = Fix $ While e s

skipSt :: Stmt
skipSt = Fix Skip

seqSt :: Stmt -> Stmt ->Stmt
seqSt a b = Fix $ Seq a b

letSt :: [Decl] -> Stmt -> Stmt
letSt dc st = Fix $ Let dc st

assignIndexSt :: Id -> Expr -> Expr -> Stmt
assignIndexSt i e e2 = Fix $ AssignIndex i e e2

assignSt :: Id -> Expr -> Stmt
assignSt i e = Fix $ Assign i e
