module Language.GCL.Parser(parse) where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Bifunctor(first)
import Data.Fix(Fix(..))
import Data.Function(on)
import Data.Functor(($>))
import Data.List(groupBy, sortOn)
import Data.Ord(Down(..))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.GCL.Opts
import Language.GCL.Syntax hiding (block, decls)
import Language.GCL.Utils((...), showT)
import Control.Applicative ((<**>))

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
  [ "Int", "Bool", "Ref"
  , "True", "False"
  , "skip", "assume", "assert"
  , "if", "else", "while", "let"
  , "forall", "exists"
  , "null", "val", "new", "H"
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
  <|> symbol "Ref" $> Ref

type' :: Parser Type
type' = primType <|> Array <$> btwn "[" "]" primType

exprAtom :: Parser Expr
exprAtom =
  choice
  [ Fix . IntLit <$> lexeme L.decimal
  , Fix (BoolLit True) <$ symbol "True"
  , Fix (BoolLit False) <$ symbol "False"
  , Fix Null <$ symbol "null"
  , try $ Fix ... Subscript <$> (Fix . Var <$> ident) <*> btwn "[" "]" expr
  , ident <**> option (Fix . Var) (try $ symbol "." *> symbol "val" $> Fix . GetVal)
  , Fix . Length <$> (symbol "#" *> ident)
  , Fix ... Forall <$> (symbol "forall" *> ident <* symbol ".") <*> expr
  , Fix ... Exists <$> (symbol "exists" *> ident <* symbol ".") <*> expr
  , btwn "(" ")" expr
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ Prefix $ Fix . Negate <$ symbol "-"
  , Prefix $ Fix . Not <$ symbol "!"
  ]
  : ((mkOp <$>) <$> grouped [minBound ..])
  where
    grouped =
      fmap (sortOn $ Down . length . show)
      . groupBy ((==) `on` precedence)
      . sortOn (Down . precedence)

    mkOp op = infix' (associativity op) $ Fix ... Op op <$ symbol (showT op)
    infix' = \case
      L -> InfixL
      R -> InfixR
      N -> InfixN

expr :: Parser Expr
expr = makeExprParser exprAtom operatorTable

stmtSimple :: Parser Stmt
stmtSimple =
  choice
  [ Fix Skip <$ symbol "skip"
  , Fix . Assume <$> (symbol "assume" *> expr)
  , Fix . Assert <$> (symbol "assert" *> expr)
  , try $ (Fix .) ... AssignIndex <$> ident <*> (btwn "[" "]" expr <* symbol "=") <*> expr
  , try $ Fix ... AssignVal <$> ident <* symbol "." <* symbol "val" <* symbol "=" <*> expr
  , try $ Fix ... AssignNew <$> ident <* symbol "=" <* symbol "new" <*> expr
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
  <*> pure 0

parse :: Opts -> Text -> Either Text Program
parse Opts{..} =
  first (("Parser error:\n" <>) . T.pack . errorBundlePretty)
  . runParser (optional sc *> program <* eof) path
