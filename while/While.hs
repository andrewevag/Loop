module While where

import qualified Loop as L
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data WhileProgram = Assignment L.Variable L.AssignmentType
                  | WhileScheme L.Variable WhileProgram
                  | Concatenation WhileProgram WhileProgram
                  deriving (Eq, Show)


languageDef = 
    emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd   = "*/",
        Token.nestedComments = True,
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = [
            "while",
            "do"
            "end"
        ],
        Token.reservedOpNames = [
            ":=",
            "+",
            "-",
            "!="
            ";"
        ]
    }

lexer = Token.makeTokenParser languageDef

-- All the individual parsers for parsing lexemes
identifier  = Token.identifier      lexer
reserved    = Token.reserved        lexer
reservedOp  = Token.reservedOp      lexer
naural      = Token.natural         lexer
semicolon   = Token.semi            lexer
whiteSpace  = Token.whiteSpace      lexer

assignment :: Parser WhileProgram
assignment = (\L.Assignment v t -> Assignment v t) <$> L.assignment

whileScheme :: Parser WhileProgram
whileScheme = do
    reserved "while"
    v <- identifier
    reservedOp "!="
    reserved "do"
    body <- whileProgram
    reserved "end"

whileProgram :: Parser WhileProgram
whileProgram = foldl1 Concatenation <$> many1 (try assign <|> whileScheme)
             
    


