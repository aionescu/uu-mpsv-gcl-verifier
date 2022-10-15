module Language.GCL.Syntax where
import Data.Fix(Fix(..))
import Data.Functor.Classes(Show1(..), showsPrec1, Eq1 (liftEq))
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
  | Conditional e e e
  deriving (Functor, Foldable, Traversable)

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
    Conditional c e1 e2 -> showText "(" . showE 0 c . showText " -> " . showE 0 e1 . showText "|" . showE 0 e2 . showText ")"

instance {-# OVERLAPPING #-} Show Expr where
  showsPrec :: Int -> Expr -> ShowS
  showsPrec p = showsPrec1 p . unFix

instance Show Decl where
  show (Decl v t) = unpack v <> ": " <> show t

decls :: [Decl] -> String
decls ds = intercalate ", " $ show <$> ds

block :: ShowS -> ShowS
block s = showText " {\n" . s . showText "}"

instance Show1 StmtF where
  liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> StmtF a -> ShowS
  liftShowsPrec showS _ _ = \case
    Skip -> showText "skip;"
    Assume e -> showText "assume " . shows e . showText ";"
    Assert e -> showText "assert " . shows e . showText ";"
    Assign v e -> showText v . showText " = " . shows e . showText ";"
    AssignIndex v i e -> showText v . showText "[" . shows i . showText "] = " . shows e . showText ";"
    If e s s2 -> showText "if " . shows e . (block . showS 0) s . showText " else" . (block . showS 0) s2
    While e s -> showText "while " . shows e . (block . showS 0) s
    Seq s s2 -> showS 0 s . showText "\n" . showS 0 s2
    Let ds s -> showText "let " . (showString . decls) ds . (block . showS 0) s

instance {-# OVERLAPPING #-} Show Stmt where
  showsPrec :: Int -> Stmt -> ShowS
  showsPrec p = showsPrec1 p . unFix


instance Show Program where
  show (Program n i o b) = unpack n <> "(" <> decls i <> ") -> " <> show o <>  "{\n" <> show b <>  "}"

instance Eq1 ExprF where
  liftEq :: (a -> b -> Bool) -> ExprF a -> ExprF b -> Bool
  liftEq _ (IntLit a) (IntLit b) = a == b
  liftEq _ (BoolLit a) (BoolLit b) = a == b
  liftEq _ (Var a) (Var b) = a == b
  liftEq _ (Length a) (Length b) = a == b
  liftEq f (BinOp o1 a1 b1) (BinOp o2 a2 b2) = o1 == o2 && f a1 a2 && f b1 b2 
  liftEq f (Negate a) (Negate b) = f a b
  liftEq f (Subscript i1 a) (Subscript i2 b) = f a b && i1 == i2
  liftEq f (Forall i1 a) (Forall i2 b) = f a b && i1 == i2
  liftEq f (Exists i1 a) (Exists i2 b) = f a b && i1 == i2
  liftEq f (Conditional a1 a2 a3) (Conditional b1 b2 b3) = f a1 b1 && f a2 b2 && f a3 b3 
  liftEq _ _ _ = False
