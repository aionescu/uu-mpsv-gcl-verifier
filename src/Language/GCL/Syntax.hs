module Language.GCL.Syntax where

import Data.Text (Text)

type Id = Text

data Type
  = Int
  | Bool
  | Array Type
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Implies
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  deriving (Eq, Show)

data Expr
  = IntLit Int
  | BoolLit Bool
  | Var Id
  | Length Id
  | BinOp BinOp Expr Expr
  | Negate Expr
  | Subscript Id Expr
  | Forall Id Expr
  | Exists Id Expr
  deriving (Show)

type Pred = Expr

data Decl = Decl
  { declName :: Id,
    declType :: Type
  }
  deriving (Show)

data Stmt
  = Skip
  | Assume Expr
  | Assert Expr
  | Assign Id Expr
  | AssignIndex Id Expr Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Seq Stmt Stmt
  | Let [Decl] Stmt
  deriving (Show)

data Program = Program
  { programName :: Id,
    programInputs :: [Decl],
    programOutput :: Decl,
    programBody :: Stmt
  }
  deriving (Show)

-- Helpers

instance Num Expr where
  fromInteger = IntLit . fromInteger
  (+) = BinOp Add
  (-) = BinOp Sub
  (*) = BinOp Mul
  negate = Negate
  abs = undefined
  signum = undefined

instance Fractional Expr where
  (/) = BinOp Div
  fromRational = undefined

true, false :: Expr
true = BoolLit True
false = BoolLit False

pattern (:&&) :: Expr -> Expr -> Expr
pattern (:&&) a b = BinOp And a b
infixr 3 :&&

pattern (:||) :: Expr -> Expr -> Expr
pattern (:||) a b = BinOp Or a b
infixr 2 :||

pattern (:=>) :: Expr -> Expr -> Expr
pattern (:=>) a b = BinOp Implies a b
infixr 1 :=>
