module Language.GCL.Parser(parse) where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Bifunctor(first)
import Data.Function(on)
import Data.Functor(($>))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.GCL.Syntax

type Parser = Parsec Void Text

lineComm :: Parser ()
lineComm = L.skipLineComment "--"

blockComm :: Parser ()
blockComm = L.skipBlockCommentNested "{-" "-}"

sc :: Parser ()
sc = L.space space1 lineComm blockComm

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

btwn :: Text -> Text -> Parser a -> Parser a
btwn = between `on` symbol

reserved :: [Text]
reserved =
  [ "Int", "Bool"
  , "True", "False"
  , "skip", "assume", "assert"
  , "if", "else", "while", "let"
  , "forall", "exists"
  ]

ident :: Parser Text
ident =
  try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier")
  where
    fstChar = letterChar <|> char '_'
    sndChar = alphaNumChar <|> char '_' <|> char '\''

    notReserved i
      | i `elem` reserved = fail $ "Reserved identifier " <> show i
      | otherwise = pure i

primType :: Parser Type
primType =
  symbol "Int" $> Int
  <|> symbol "Bool" $> Bool

type' :: Parser Type
type' = primType <|> Array <$> btwn "[" "]" primType

exprAtom :: Parser Expr
exprAtom =
  choice
  [ IntLit <$> lexeme L.decimal
  , BoolLit True <$ symbol "True"
  , BoolLit False <$ symbol "False"
  , try $ Subscript <$> ident <*> btwn "[" "]" expr
  , Var <$> ident
  , Length <$> (char '#' *> ident)
  , Forall <$> (symbol "forall" *> ident <* symbol ".") <*> expr
  , Exists <$> (symbol "exists" *> ident <* symbol ".") <*> expr
  , btwn "(" ")" expr
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix $ symbol "~" $> Negate ]
  , [ opL "*" Mul, opL "/" Div ]
  , [ opL "+" Add, opL "-" Sub ]
  , [ opN "<=" Lte, opN ">=" Gte, opN "<" Lt, opN ">" Gt ]
  , [ opN "==" Eq, opN "/=" Neq ]
  , [ opR "&&" And ]
  , [ opR "||" Or ]
  , [ opN "=>" Implies ]
  ]
  where
    op f sym op = f $ symbol sym $> BinOp op
    opL = op InfixL
    opR = op InfixR
    opN = op InfixN

expr :: Parser Expr
expr = makeExprParser exprAtom operatorTable

stmtSimple :: Parser Stmt
stmtSimple =
  choice
  [ Skip <$ symbol "skip"
  , Assume <$> (symbol "assume" *> expr)
  , Assert <$> (symbol "assert" *> expr)
  , try $ AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  , Assign <$> (ident <* symbol "=") <*> expr
  ] <* symbol ";"

stmtCompound :: Parser Stmt
stmtCompound =
  choice
  [ If <$> (symbol "if" *> expr) <*> block <*> ((symbol "else" *> block) <|> pure Skip)
  , While <$> (symbol "while" *> expr) <*> block
  , Let <$> (symbol "let" *> decls) <*> block
  , block
  ] <* optional (symbol ";")

stmt :: Parser Stmt
stmt = stmtSimple <|> stmtCompound

block :: Parser Stmt
block = btwn "{" "}" $ seq <$> many stmt
  where
    seq [] = Skip
    seq xs = foldr1 Seq xs

decl :: Parser Decl
decl = Decl <$> (ident <* symbol ":") <*> type'

decls :: Parser [Decl]
decls = decl `sepEndBy` symbol ","

program :: Parser Program
program =
  Program
  <$> ident
  <*> btwn "(" ")" decls
  <*> (symbol "->" *> decl)
  <*> block

parse :: FilePath -> Text -> Either Text Program
parse path =
  first (("Parser error:\n" <>) . T.pack . errorBundlePretty)
  . runParser (optional sc *> program <* eof) path
