module CpsH.Lexer where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language

import Types

lexer :: GenTokenParser StreamP UserStateP MonadP
lexer = makeTokenParser style

style :: GenLanguageDef StreamP UserStateP MonadP
style = LanguageDef
        { commentStart    = ""
        , commentEnd      = ""
        , commentLine     = ""
        , nestedComments  = True
        , identStart      = letter <|> char '_'
        , identLetter     = alphaNum <|> oneOf "_'"
        , opStart         = opLetter style
        , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , reservedOpNames = [":","=","+"]
        , reservedNames   = ["def","return"]
        , caseSensitive   = True
        }

identifier     = Text.Parsec.Token.identifier     lexer
reserved       = Text.Parsec.Token.reserved       lexer
operator       = Text.Parsec.Token.operator       lexer
reservedOp     = Text.Parsec.Token.reservedOp     lexer
charLiteral    = Text.Parsec.Token.charLiteral    lexer
stringLiteral  = Text.Parsec.Token.stringLiteral  lexer
natural        = Text.Parsec.Token.natural        lexer
integer        = Text.Parsec.Token.integer        lexer
float          = Text.Parsec.Token.float          lexer
naturalOrFloat = Text.Parsec.Token.naturalOrFloat lexer
decimal        = Text.Parsec.Token.decimal        lexer
hexadecimal    = Text.Parsec.Token.hexadecimal    lexer
octal          = Text.Parsec.Token.octal          lexer
symbol         = Text.Parsec.Token.symbol         lexer
lexeme         = Text.Parsec.Token.lexeme         lexer
whiteSpace     = Text.Parsec.Token.whiteSpace     lexer
parens         = Text.Parsec.Token.parens         lexer
braces         = Text.Parsec.Token.braces         lexer
angles         = Text.Parsec.Token.angles         lexer
brackets       = Text.Parsec.Token.brackets       lexer
squares        = Text.Parsec.Token.squares        lexer
semi           = Text.Parsec.Token.semi           lexer
comma          = Text.Parsec.Token.comma          lexer
colon          = Text.Parsec.Token.colon          lexer
dot            = Text.Parsec.Token.dot            lexer
semiSep        = Text.Parsec.Token.semiSep        lexer
semiSep1       = Text.Parsec.Token.semiSep1       lexer
commaSep       = Text.Parsec.Token.commaSep       lexer
commaSep1      = Text.Parsec.Token.commaSep1      lexer
