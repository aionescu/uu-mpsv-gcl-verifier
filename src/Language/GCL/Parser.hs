module Language.GCL.Parser(parse) where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Bifunctor(first)
import Data.Fix(Fix(..))
import Data.Function(on)
import Data.Functor(($>))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.GCL.Opts
import Language.GCL.Syntax hiding (block, decls)
import Language.GCL.Utils((...))

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
  [ Fix . IntLit <$> lexeme L.decimal
  , Fix (BoolLit True) <$ symbol "True"
  , Fix (BoolLit False) <$ symbol "False"
  , try $ Fix ... Subscript <$> ident <*> btwn "[" "]" expr
  , Fix . Var <$> ident
  , Fix . Length <$> (char '#' *> ident)
  , Fix ... Forall <$> (symbol "forall" *> ident <* symbol ".") <*> expr
  , Fix ... Exists <$> (symbol "exists" *> ident <* symbol ".") <*> expr
  , btwn "(" ")" expr
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ Prefix $ Fix . Negate <$ symbol "-" ]
  , [ Prefix $ Fix . Not <$ symbol "~" ]
  , [ opL "*" Mul, opL "/" Div ]
  , [ opL "+" Add, opL "-" Sub ]
  , [ opN "<=" Lte, opN ">=" Gte, opN "<" Lt, opN ">" Gt ]
  , [ opN "==" Eq, opN "/=" Neq ]
  , [ opR "&&" And ]
  , [ opR "||" Or ]
  , [ opN "=>" Implies ]
  ]
  where
    op f sym op = f $ Fix ... Op op <$ symbol sym
    opL = op InfixL
    opR = op InfixR
    opN = op InfixN

expr :: Parser Expr
expr = makeExprParser exprAtom operatorTable

stmtSimple :: Parser Stmt
stmtSimple =
  choice
  [ Fix Skip <$ symbol "skip"
  , Fix . Assume <$> (symbol "assume" *> expr)
  , Fix . Assert <$> (symbol "assert" *> expr)
  , try $ (Fix .) ... AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  , Fix ... Assign <$> (ident <* symbol "=") <*> expr
  ] <* symbol ";"

stmtCompound :: Parser Stmt
stmtCompound =
  choice
  [ (Fix .) ... If <$> (symbol "if" *> expr) <*> block <*> option (Fix Skip) (symbol "else" *> block)
  , Fix ... While <$> (symbol "while" *> expr) <*> block
  , Fix ... Let <$> (symbol "let" *> decls) <*> block
  , block
  ] <* optional (symbol ";")

stmt :: Parser Stmt
stmt = stmtSimple <|> stmtCompound

block :: Parser Stmt
block = btwn "{" "}" $ seq <$> many stmt
  where
    seq [] = Fix Skip
    seq xs = foldr1 (Fix ... Seq) xs

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

parse :: Opts -> Text -> Either Text Program
parse Opts{..} =
  first (("Parser error:\n" <>) . T.pack . errorBundlePretty)
  . runParser (optional sc *> program <* eof) path
