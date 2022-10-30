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
  | Array Type
  deriving Eq

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
  | Var Id
  | Length Id
  | Op Op e e
  | Negate e
  | Not e
  | Subscript Id e
  | Forall Id e
  | Exists Id e
  | Conditional e e e
  deriving (Functor, Foldable, Traversable, Eq)

instance {-# OVERLAPPING #-} Eq Expr where
  Fix a == Fix b = a == b

type Expr = Fix ExprF

type Pred = Expr

data Decl = Decl
  { declName :: Id,
    declType :: Type
  }

data StmtF e
  = Skip
  | Assume Expr
  | Assert Expr
  | Assign Id Expr
  | AssignIndex Id Expr Expr
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
    Var v -> showText v
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
    Subscript v e -> showText v . showChar '[' . showE 0 e . showChar ']'
    Forall v e -> showParen (p > 1) $ showString "forall " . showText v . showString ". " . showE 0 e
    Exists v e -> showParen (p > 1) $ showString "exists " . showText v . showString ". " . showE 0 e
    Conditional c e1 e2 -> showText "(" . showE 0 c . showText " -> " . showE 0 e1 . showText "|" . showE 0 e2 . showText ")"

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

  showList l s = (show =<< l) <> s

instance Show Program where
  show (Program n i o b) = unpack n <> "(" <> decls i <> ") -> " <> show o <> block b
