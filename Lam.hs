module Lam where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr
import Control.Monad.Reader

import Data.Maybe
import Data.Void

data Lam
  = Lam String Lam
  | App Lam Lam
  | Add Lam Lam
  | Var String
  | Int Int
  deriving Show

-----------------
-- Interpreter --
-----------------

type Env = [(String,Dom)]
type M = ReaderT Env Maybe
data Dom = INT Int | FUN (Dom -> M Dom)

instance Show Dom where
  show (INT i) = show i
  show (FUN _) = "fun"

lam2dom :: Lam -> Dom
lam2dom = fromJust . flip runReaderT [] . l2d

l2d :: Lam -> M Dom
l2d (Lam x e) = do
  nv <- ask
  return $ FUN $ \ v -> local (const ((x,v):nv)) (l2d e)
l2d (App e1 e2) = do
  FUN v1 <- l2d e1
  v2 <- l2d e2
  v1 v2
l2d (Add e1 e2) = do
  INT i1 <- l2d e1
  INT i2 <- l2d e2
  return $ INT $ i1 + i2
l2d (Var x) = ReaderT $ lookup x
l2d (Int i) = return $ INT i

---------------------
-- Pretty Printing --
---------------------

pprint (App e1 e2) = pprint e1 ++ " " ++
  case e2 of
    App {} -> "(" ++ pprint e2 ++ ")"
    _      -> pprint e2
pprint (Lam x e) = "(\\ " ++ x ++ go e
  where go e = case e of
          Lam x e -> " " ++ x ++ go e
          _       -> " -> " ++ pprint e ++ ")"
pprint (Var x) = x
pprint (Int i) = show i
pprint (Add e1 e2) = pprint e1 ++ " + " ++
    case e2 of
      App {} -> "(" ++ pprint e2 ++ ")"
      _      -> pprint e2

------------
-- Parser --
------------

type Parser = Parsec Void String

str2lam :: String -> Lam
str2lam str =
  case parse (between (optional space1) eof expr) "str2lam parse error" str of
    Left err -> error $ errorBundlePretty err
    Right lam -> lam

expr :: Parser Lam
expr = makeExprParser term table

table :: [[Operator Parser Lam]]
table =
  [[ InfixL $ App <$ symbol ""  ],
   [ InfixL $ Add <$ symbol "+" ]]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

term :: Parser Lam
term = choice
  [ letbind
  , lam
  , var
  , int
  , parens expr
  ]

letbind :: Parser Lam
letbind = do
  symbol "let"
  x <- word
  symbol "="
  e <- expr
  symbol "in"
  t <- expr
  return $ App (Lam x t) e

lam :: Parser Lam
lam = do
  symbol "\\"
  xs <- some word
  symbol "->"
  e <- expr
  return $ foldr Lam e xs

var :: Parser Lam
var = Var <$> word

int :: Parser Lam
int = Int <$> decimal

keywords :: [String]
keywords = ["let", "in"]

word :: Parser String
word = do
  notFollowedBy (choice $ symbol <$> keywords)
  lexeme $ (:) <$> letterChar <*> many alphaNumChar

decimal :: Parser Int
decimal = lexeme L.decimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty
