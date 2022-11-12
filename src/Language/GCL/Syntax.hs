module Language.GCL.Syntax where

import Control.Category((>>>))
import Data.Fix(Fix(..))
import Data.Functor.Classes(Show1(..), showsPrec1)
import Data.List(intercalate)
import Data.Text(Text, unpack)

type Id = Text

data Type
  = Int
  | Bool
  | Ref
  | Array Type
  deriving (Eq, Ord)

data Op
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
  deriving (Eq, Enum, Bounded)

precedence :: Op -> Int
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

data Assoc = L | R | N
  deriving (Eq, Show)

associativity :: Op -> Assoc
associativity = \case
  Add -> L
  Sub -> L
  Mul -> L
  Div -> L
  And -> R
  Or -> R
  Implies -> N
  Eq -> N
  Neq -> N
  Lt -> N
  Lte -> N
  Gt -> N
  Gte -> N

data ExprF e
  = IntLit Int
  | BoolLit Bool
  | Null
  | Var Id
  | GetVal Id
  | Length Id
  | Op Op e e
  | Negate e
  | Not e
  | Subscript e e
  | Forall Id e
  | Exists Id e
  | Cond e e e
  | RepBy e e e
  deriving (Functor, Foldable, Traversable, Eq)

instance {-# OVERLAPPING #-} Eq Expr where
  Fix a == Fix b = a == b

type Expr = Fix ExprF

type Pred = Expr

data Decl = Decl
  { declName :: Id,
    declType :: Type
  }
  deriving Eq

data StmtF e
  = Skip
  | Assume Expr
  | Assert Expr
  | Assign Id Expr
  | AssignIndex Id Expr Expr
  | AssignNew Id Expr
  | AssignVal Id Expr
  | If Expr e e
  | While Expr e
  | Seq e e
  | Let [Decl] e
  deriving (Functor, Foldable, Traversable)

type Stmt = Fix StmtF

-- Linear statement
data LStmt
  = LAssume Expr
  | LAssert Expr
  | LAssign Id Expr
  | LAssignIndex Id Expr Expr
  | LAssignNew Id Expr
  | LAssignVal Id Expr
  deriving Eq

-- Linear path
type LPath = [LStmt]

data Program = Program
  { programName :: Id,
    programInputs :: [Decl],
    programOutput :: Decl,
    programBody :: Stmt
  }

-- Show instances

instance Show Type where
  show = \case
    Int -> "Int"
    Bool -> "Bool"
    Ref -> "Ref"
    Array a -> "[" <> show a <> "]"

instance Show Op where
  show = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    And -> "&&"
    Or -> "||"
    Implies -> "=>"
    Eq -> "=="
    Neq -> "!="
    Lt -> "<"
    Lte -> "<="
    Gt -> ">"
    Gte -> ">="

showText :: Text -> ShowS
showText = showString . unpack

instance Show1 ExprF where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> ExprF a -> ShowS
  liftShowsPrec showE _ p = \case
    IntLit i -> shows i
    BoolLit b -> shows b
    Null -> showString "null"
    Var v -> showText v
    GetVal v -> showText v . showString ".val"
    Length v -> showChar '#' . showText v
    Op o a b -> showParen (p > q) $ showE ql a . showChar ' ' . shows o . showChar ' ' . showE qr b
      where
        q = precedence o
        (ql, qr) = case associativity o of
          L -> (q, q + 1)
          R -> (q + 1, q)
          N -> (q + 1, q + 1)
    Negate e -> showChar '-' . showE 9 e
    Not e -> showChar '!' . showE 9 e
    Subscript v e -> showE 0 v . showChar '[' . showE 0 e . showChar ']'
    Forall v e -> showParen (p > 1) $ showString "forall " . showText v . showString ". " . showE 0 e
    Exists v e -> showParen (p > 1) $ showString "exists " . showText v . showString ". " . showE 0 e
    Cond c e1 e2 -> showString "(" . showE 0 c . showString " -> " . showE 0 e1 . showString "|" . showE 0 e2 . showString ")"
    RepBy v i e -> showE 0 v . showString "(" . showE 0 i . showString " repby " . showE 0 e . showString ")"

instance {-# OVERLAPPING #-} Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec p = showsPrec1 p . unFix

instance Show Decl where
  show (Decl v t) = unpack v <> ": " <> show t

decls :: [Decl] -> String
decls ds = intercalate ", " $ show <$> ds

block :: Stmt -> String
block (Fix Skip) = " { }"
block s = " {\n" <> unlines (("  " <>) <$> lines (show s)) <> "}"

instance {-# OVERLAPPING #-} Show Stmt where
  show = unFix >>> \case
    Skip -> "skip;"
    Assume e -> "assume " <> show e <> ";"
    Assert e -> "assert " <> show e <> ";"
    Assign v e -> unpack v <> " = " <> show e <> ";"
    AssignIndex v i e -> unpack v <> "[" <> show i <> "] = " <> show e <> ";"
    AssignNew v e -> unpack v <> " = new " <> show e <> ";"
    AssignVal v e -> unpack v <> ".val = " <> show e <> ";"
    If e s (Fix Skip) -> "if " <> show e <> block s
    If e s s2 -> "if " <> show e <> block s <> " else" <> block s2
    While e s -> "while " <> show e <> block s
    Seq s s2 -> show s <> "\n" <> show s2
    Let ds s -> "let " <> decls ds <> block s

instance Show LStmt where
  show e = show $ Fix $ case e of
    LAssume e -> Assume e
    LAssert e -> Assert e
    LAssign v e -> Assign v e
    LAssignIndex v i e -> AssignIndex v i e
    LAssignNew v e -> AssignNew v e
    LAssignVal v e -> AssignVal v e

  showList l s = unwords (show <$> l) <> s

instance Show Program where
  show (Program n i o b) = unpack n <> "(" <> decls i <> ") -> " <> show o <> block b
