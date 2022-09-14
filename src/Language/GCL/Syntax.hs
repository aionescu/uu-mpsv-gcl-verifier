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
  = Add | Sub | Mul | Div
  | And | Or | Implies
  | Eq | Neq
  | Lt | Lte
  | Gt | Gte
  deriving Eq

data Expr
  = IntLit Int
  | BoolLit Bool
  | Var Id
  | Length Id
  | BinOp Expr BinOp Expr
  | Negate Expr
  | Subscript Id Expr
  | Forall Id Expr
  | Exists Id Expr

data Decl =
  Decl
  { declName :: Id
  , declType :: Type
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

data Program =
  Program
  { programName :: Id
  , programInputs :: [Decl]
  , programOutput :: Decl
  , programBody :: Stmt
  }

-- `Show` Instances

instance Show Type where
  show Int = "Int"
  show Bool = "Bool"
  show (Array a) = "[" <> show a <> "]"

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show And = "&&"
  show Or = "||"
  show Implies = "=>"
  show Eq = "=="
  show Neq = "/="
  show Lt = "<"
  show Lte = "<="
  show Gt = ">"
  show Gte = ">="

parens :: Bool -> String -> String
parens False s = s
parens True s = "(" <> s <> ")"

showE :: Bool -> Expr -> String
showE _ (IntLit i) = show i
showE _ (BoolLit b) = show b
showE _ (Var v) = T.unpack v
showE _ (Length v) = "#" <> T.unpack v
showE p (BinOp a o b) = parens p $ showE True a <> " " <> show o <> " " <> showE True b
showE _ (Negate e) = "~" <> showE True e
showE _ (Subscript v e) = T.unpack v <> "[" <> show e <> "]"
showE p (Forall v e) = parens p $ "forall " <> T.unpack v <> ". " <> show e
showE p (Exists v e) = parens p $ "exists " <> T.unpack v <> ". " <> show e

instance Show Expr where
  show = showE False

instance Show Decl where
  show (Decl v t) = T.unpack v <> ": " <> show t

decls :: [Decl] -> String
decls ds = intercalate ", " $ show <$> ds

block :: Stmt -> String
block Skip = " { }"
block s = " {\n" <> unlines (("  " <>) <$> lines (show s)) <> "}"

instance Show Stmt where
  show Skip = "skip;"
  show (Assume e) = "assume " <> show e <> ";"
  show (Assert e) = "assert " <> show e <> ";"
  show (Assign v e) = T.unpack v <> " = " <> show e <> ";"
  show (AssignIndex v i e) = T.unpack v <> "[" <> show i <> "] = " <> show e <> ";"
  show (If e s Skip) = "if " <> show e <> block s
  show (If e s s2) = "if " <> show e <> block s <> " else" <> block s2
  show (While e s) = "while " <> show e <> block s
  show (Seq s s2) = show s <> "\n" <> show s2
  show (Let ds s) = "let " <> decls ds <> block s

instance Show Program where
  show (Program n i o b) = T.unpack n <> "(" <> decls i <> ") -> " <> show o <> block b
