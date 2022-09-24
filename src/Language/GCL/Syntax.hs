module Language.GCL.Syntax where

import Data.List(intercalate)
import Data.Text(Text)
import Data.Text qualified as T

type Id = Text

data Type
  = Int
  | Bool
  | Array Type
  deriving Eq

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
  deriving Eq

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

type Pred = Expr

data Decl = Decl
  { declName :: Id,
    declType :: Type
  }

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

data Program = Program
  { programName :: Id,
    programInputs :: [Decl],
    programOutput :: Decl,
    programBody :: Stmt
  }

-- Helpers for cleaner syntax

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

-- Show instances

instance Show Type where
  show = \case
    Int -> "Int"
    Bool -> "Bool"
    Array a -> "[" <> show a <> "]"

instance Show BinOp where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    And -> "&&"
    Or -> "||"
    Implies -> "=>"
    Eq -> "=="
    Neq -> "/="
    Lt -> "<"
    Lte -> "<="
    Gt -> ">"
    Gte -> ">="

precedence :: BinOp -> Int
precedence = \case
  Mul -> 8
  Div -> 8
  Add -> 7
  Sub -> 7
  Eq -> 6
  Neq -> 6
  Lt -> 5
  Lte -> 5
  Gt -> 5
  Gte -> 5
  And -> 4
  Or -> 3
  Implies -> 2

showText :: Text -> ShowS
showText = showString . T.unpack

instance Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec p = \case
    IntLit i -> shows i
    BoolLit b -> shows b
    Var v -> showText v
    Length v -> showChar '#' . showText v
    BinOp o a b ->
      let q = precedence o
      in showParen (p > q) $ showsPrec q a . showChar ' ' . shows o . showChar ' ' . showsPrec q b
    Negate e -> showChar '~' . showsPrec 9 e
    Subscript v e -> showText v . showChar '[' . shows e . showChar ']'
    Forall v e -> showParen (p > 1) $ showString "forall " . showText v . showString ". " . shows e
    Exists v e -> showParen (p > 1) $ showString "exists " . showText v . showString ". " . shows e

instance Show Decl where
  show (Decl v t) = T.unpack v <> ": " <> show t

decls :: [Decl] -> String
decls ds = intercalate ", " $ show <$> ds

block :: Stmt -> String
block Skip = " { }"
block s = " {\n" <> unlines (("  " <>) <$> lines (show s)) <> "}"

instance Show Stmt where
  show = \case
    Skip -> "skip;"
    Assume e -> "assume " <> show e <> ";"
    Assert e -> "assert " <> show e <> ";"
    Assign v e -> T.unpack v <> " = " <> show e <> ";"
    AssignIndex v i e -> T.unpack v <> "[" <> show i <> "] = " <> show e <> ";"
    If e s Skip -> "if " <> show e <> block s
    If e s s2 -> "if " <> show e <> block s <> " else" <> block s2
    While e s -> "while " <> show e <> block s
    Seq s s2 -> show s <> "\n" <> show s2
    Let ds s -> "let " <> decls ds <> block s

instance Show Program where
  show (Program n i o b) = T.unpack n <> "(" <> decls i <> ") -> " <> show o <> block b
