module Language.GCL.Syntax where
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

data ExprF e
  = IntLit Int
  | BoolLit Bool
  | Var Id
  | Length Id
  | BinOp BinOp e e
  | Negate e
  | Subscript Id e
  | Forall Id e
  | Exists Id e
  deriving Functor

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
showText = showString . unpack

instance Show1 ExprF where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> ExprF a -> ShowS
  liftShowsPrec showE _ p = \case
    IntLit i -> shows i
    BoolLit b -> shows b
    Var v -> showText v
    Length v -> showChar '#' . showText v
    BinOp o a b ->
      let q = precedence o
      in showParen (p > q) $ showE q a . showChar ' ' . shows o . showChar ' ' . showE q b
    Negate e -> showChar '~' . showE 9 e
    Subscript v e -> showText v . showChar '[' . showE 0 e . showChar ']'
    Forall v e -> showParen (p > 1) $ showString "forall " . showText v . showString ". " . showE 0 e
    Exists v e -> showParen (p > 1) $ showString "exists " . showText v . showString ". " . showE 0 e

instance {-# OVERLAPPING #-} Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec p = showsPrec1 p . unFix

instance Show Decl where
  show (Decl v t) = unpack v <> ": " <> show t

decls :: [Decl] -> String
decls ds = intercalate ", " $ show <$> ds

-- block :: StmtF a -> String
-- block = \case 
--   Skip -> " { }"
--   s -> " {\n" <> unlines (("  " <>) <$> lines (show s)) <> "}"

-- instance  Traversable StmtF 

-- instance Show Stmt where
--   show = \case
--     Skip -> "skip;"
--     Assume e -> "assume " <> show e <> ";"
--     Assert e -> "assert " <> show e <> ";"
--     Assign v e -> unpack v <> " = " <> show e <> ";"
--     AssignIndex v i e -> unpack v <> "[" <> show i <> "] = " <> show e <> ";"
--     If e s Skip -> "if " <> show e <> block s
--     If e s s2 -> "if " <> show e <> block s <> " else" <> block s2
--     While e s -> "while " <> show e <> block s
--     Seq s s2 -> show s <> "\n" <> show s2
--     Let ds s -> "let " <> decls ds <> block s

-- instance Show Program where
--   show (Program n i o b) = unpack n <> "(" <> decls i <> ") -> " <> show o <> block b
