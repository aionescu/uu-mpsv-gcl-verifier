module Language.GCL.PrettyPrint where

import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

import Language.GCL.Syntax
import Language.GCL.Utils(showT)

class Pretty a where
  {-# MINIMAL pp | pp' #-}

  pp' :: Int -> a -> Text
  pp' _ = pp

  pp :: a -> Text
  pp = pp' 0

pPrint :: Pretty a => a -> IO ()
pPrint = T.putStrLn . pp

instance Pretty Type where
  pp Int = "Int"
  pp Bool = "Bool"
  pp (Array a) = "[" <> pp a <> "]"

instance Pretty BinOp where
  pp Add = "+"
  pp Sub = "-"
  pp Mul = "*"
  pp Div = "/"
  pp And = "&&"
  pp Or = "||"
  pp Implies = "=>"
  pp Eq = "=="
  pp Neq = "/="
  pp Lt = "<"
  pp Lte = "<="
  pp Gt = ">"
  pp Gte = ">="

precedence :: BinOp -> Int
precedence Mul = 8
precedence Div = 8
precedence Add = 7
precedence Sub = 7
precedence Eq = 6
precedence Neq = 6
precedence Lt = 5
precedence Lte = 5
precedence Gt = 5
precedence Gte = 5
precedence And = 4
precedence Or = 3
precedence Implies = 2

paren :: Int -> Int -> Text -> Text
paren p e t
  | e < p = "(" <> t <> ")"
  | otherwise = t

instance Pretty Expr where
  pp' _ (IntLit i) = showT i
  pp' _ (BoolLit b) = showT b
  pp' _ (Var v) = v
  pp' _ (Length v) = "#" <> v
  pp' p (BinOp o a b) = paren p prec $ pp' prec a <> " " <> pp o <> " " <> pp' prec b
    where
      prec = precedence o
  pp' _ (Negate e) = "~" <> pp' 9 e
  pp' _ (Subscript v e) = v <> "[" <> pp e <> "]"
  pp' p (Forall v e) = paren p 1 $ "forall " <> v <> ". " <> pp e
  pp' p (Exists v e) = paren p 1 $ "exists " <> v <> ". " <> pp e

instance Pretty Decl where
  pp (Decl v t) = v <> ": " <> pp t

decls :: [Decl] -> Text
decls ds = T.intercalate ", " $ pp <$> ds

block :: Stmt -> Text
block Skip = " { }"
block s = " {\n" <> T.unlines (("  " <>) <$> T.lines (pp s)) <> "}"

instance Pretty Stmt where
  pp Skip = "skip;"
  pp (Assume e) = "assume " <> pp e <> ";"
  pp (Assert e) = "assert " <> pp e <> ";"
  pp (Assign v e) = v <> " = " <> pp e <> ";"
  pp (AssignIndex v i e) = v <> "[" <> pp i <> "] = " <> pp e <> ";"
  pp (If e s Skip) = "if " <> pp e <> block s
  pp (If e s s2) = "if " <> pp e <> block s <> " else" <> block s2
  pp (While e s) = "while " <> pp e <> block s
  pp (Seq s s2) = pp s <> "\n" <> pp s2
  pp (Let ds s) = "let " <> decls ds <> block s

instance Pretty Program where
  pp (Program n i o b) = n <> "(" <> decls i <> ") -> " <> pp o <> block b
