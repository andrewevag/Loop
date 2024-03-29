module ReducedPascal where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map as Map
import Data.Maybe

-- Datatypes for definining the AST of Reduced Pascal

data Variable = Variable String
              | ArrayIndexedAt String Expression
              deriving (Eq, Show)
data Expression = Equals Expression Expression
                | Different Expression Expression
                | Less Expression Expression
                | LessEq Expression Expression
                | Greater Expression Expression
                | GreaterEq Expression Expression
                | Plus Expression Expression
                | Minus Expression Expression
                | Mult Expression Expression
                | Div Expression Expression
                | Mod Expression Expression
                | Constant Integer
                | Var Variable
                deriving (Eq, Show)

-- This is the datatype to hold a statement 
-- A program is basically a statement but it needs to
-- have the variable definition space handled
data Statement = Block [Statement]
               | Assignment Variable Expression
               | If Expression Statement
               | IfElse Expression Statement Statement
               | Case Expression [(Integer, Statement)]
               | For Variable (Expression, Expression) Statement
               -- in the for loop (from-lowerBound, to-UpperBound)
               deriving (Eq, Show)

-- The only two types of variables accepted in this
-- version of pascal
data PascalType = IntegerP
                | ArrayP (Integer, Integer) -- it should have a second argument here for the types of the variables in the array but is not needed since we only have Integers and will only allow arrays[integers]
                deriving (Eq, Show)

data VarDecl = IntegerVar String
             | ArrayVar String (Integer, Integer)
             deriving (Eq, Show)

-- The definition for the whole program
data PascalProgram = Program {
    name      :: String,      -- Name of the program
    variables :: [VarDecl],   -- The Variables declared at the start
    body      :: [Statement]  -- The main body (begin ... end.)
} deriving (Eq, Show)

type SymbolTable = Map.Map String PascalType


languageDef = 
    emptyDef {
        Token.commentStart   = "(*",
        Token.commentEnd     = "*)",
        Token.commentLine    = "//",
        Token.nestedComments = True,
        Token.identStart      = letter,
        Token.identLetter      = alphaNum,
        Token.reservedNames  = [
            "begin",
            "end",
            "if",
            "then",
            "else",
            "case",
            "of",
            "for",
            "to",
            "do",
            "program",
            "var",
            "integer",
            "array"
        ],
        Token.reservedOpNames = [
            "=",
            "<>",
            "<",
            ">",
            "<=",
            ">=",
            "+",
            "-",
            "*",
            "div",
            "mod",
            ":=",
            "[",
            "]",
            ",",
            ":",
            ".",
            "..."
        ]
    }

lexer = Token.makeTokenParser languageDef

-- All the individual parsers for parsing lexemes
identifier           = Token.identifier     lexer
reserved             = Token.reserved       lexer
reservedOp           = Token.reservedOp     lexer
parens               = Token.parens         lexer
natural              = Token.natural        lexer
semicolon            = Token.semi           lexer
whiteSpace           = Token.whiteSpace     lexer

-- expression parser
expression :: Parser Expression
expression = buildExpressionParser table term
             <?> "expression"

-- base case of expression parser
term :: Parser Expression
term = try (parens expression)
       <|> liftM Constant natural
       <|> try (do {id <- identifier; reservedOp "["; p <- expression ; reservedOp "]"; return $ Var (ArrayIndexedAt id p);})
       <|> liftM (Var . Variable) identifier
       <?> "simple term failed!"

table = [
            [binary "*" Mult AssocLeft, binary "div" Div AssocLeft, binary "mod" Mod AssocLeft],
            [binary "+" Plus AssocLeft, binary "-" Minus AssocLeft],
            [binary "=" Equals AssocNone, binary "<>" Different AssocNone, binary "<" Less AssocNone, binary ">" Greater AssocNone, binary "<=" LessEq AssocNone, binary ">=" GreaterEq AssocNone ] 
    ]

binary name fun assoc = Infix (do{ reservedOp name; return fun }) assoc


-- Now the parsers for Statements to build Statement Syntax Tree
statement :: Parser Statement
statement =     try block
            <|> try assignment
            <|> try ifElse
            <|> try if'
            <|> try case'
            <|> for
            <?> "Probably Something Wrong with opening Statement Here"

block :: Parser Statement
block = do
        reserved "begin"
        s <- endBy1 statement semicolon
        reserved "end"
        return $ Block s

assignment :: Parser Statement
assignment = do
             v <- variable
             reservedOp ":="
             e <- expression
             return $ Assignment v e
             <?> "Failed to parse Assignment!"

variable :: Parser Variable
variable = try (do {id <- identifier; reservedOp "["; p <- expression ; reservedOp "]"; return (ArrayIndexedAt id p);})
              <|> liftM Variable identifier
           <?> "failed to parse variable!"

ifElse :: Parser Statement
ifElse = do
         reserved "if"
         condition <- expression
         reserved "then"
         ifpart <- statement
         reserved "else"
         elsepart <- statement
         return $ IfElse condition ifpart elsepart

if' :: Parser Statement
if' = do
      reserved "if"
      condition <- expression
      reserved "then"
      ifpart <- statement
      return $ If condition ifpart

case' :: Parser Statement
case' = do
        reserved "case"
        arg <- expression
        reserved "of"
        l <- sepBy1 caseListElem semicolon
        reserved "end"
        return $ Case arg l

caseListElem :: Parser (Integer, Statement)
caseListElem = do
               n <- natural
               reservedOp ":"
               body <- statement
               return (n, body)

for :: Parser Statement
for = do
      reserved "for"
      Assignment loopVariable lowerBound <- assignment
      reserved "to"
      upperBound <- expression
      reserved "do"
      body <- statement
      return $ For loopVariable (lowerBound, upperBound) body
    


program :: Parser PascalProgram
program = do
          reserved "program"
          n <- identifier
          semicolon
          reserved "var"
          vs <- endBy1 varDef semicolon
          -- now for the main body
          Block ss <- block
          reservedOp "."
          return Program {
              name = n,
              variables = concat vs,
              body = ss
          }


varDef :: Parser [VarDecl]
varDef = do
         ids <- sepBy1 identifier (reservedOp ",")
         reservedOp ":"
         func <- type'
         return $ map func ids

-- This just gathers which type it is and returns
-- the correct constructor to apply to the names
type' :: Parser (String -> VarDecl)
type' =     (try $ reserved "integer" >> return IntegerVar)
        <|> do
            reserved "array"
            reservedOp "["
            from <- natural
            reservedOp "..."
            to <- natural
            reservedOp "]"
            reserved "of"
            reserved "integer"
            return $ flip ArrayVar (from, to)


sem :: PascalProgram -> Either SymbolTable String
sem p = 
    let 
        st = getSymbolTable p 
        programStatements = body p
    in
        case sem' (Right (Block programStatements)) st of
            Nothing  -> Left st
            Just err -> Right err

getSymbolTable :: PascalProgram -> SymbolTable
getSymbolTable p = foldl func Map.empty (variables p)
    where
        func prev (IntegerVar v) | isNothing $ Map.lookup v prev = Map.insert v IntegerP prev
                                 | otherwise                     = error $ varString v ++ "already defined"
        func prev (ArrayVar v bounds) | isNothing $ Map.lookup v prev = Map.insert v (ArrayP bounds) prev
                                      | otherwise                     = error $ varString v ++ "already defined"


sem' :: Either Expression Statement -> SymbolTable -> Maybe String
sem' (Left (Constant c)) st = Nothing
sem' (Left (Var (Variable v))) st | v `Map.notMember` st = Just $ varString v ++ "is not previously defined"
                                  | otherwise            =
                                      case Map.lookup v st of
                                          Just (ArrayP _) -> Just $ varString v ++ "is defined as an array and cannot be used in expressions"
                                          Just (IntegerP) -> Nothing
                                          Nothing         -> Just "should not have gotten here"

sem' (Left (Var (ArrayIndexedAt v e))) st = 
    case Map.lookup v st of
        Just (ArrayP (_, _)) -> Nothing
        Just _                  -> Just $ varString v ++ "is not an array to be indexed"
        Nothing                 -> Just $ varString v ++ "is not previously defined"

sem' (Left (Equals l r))    st = helper [Left l, Left r] st
sem' (Left (Different l r)) st = helper [Left l, Left r] st
sem' (Left (Less l r))      st = helper [Left l, Left r] st
sem' (Left (LessEq l r))    st = helper [Left l, Left r] st
sem' (Left (Greater l r))   st = helper [Left l, Left r] st
sem' (Left (GreaterEq l r)) st = helper [Left l, Left r] st
sem' (Left (Plus l r))      st = helper [Left l, Left r] st
sem' (Left (Minus l r))     st = helper [Left l, Left r] st
sem' (Left (Mult l r))      st = helper [Left l, Left r] st
sem' (Left (Div l r))       st = helper [Left l, Left r] st
sem' (Left (Mod l r))       st = helper [Left l, Left r] st
    

-- Starting to semantically analyse statements
sem' (Right (Block stms)) st = helper (map Right stms) st

sem' (Right (Assignment v e)) st = helper [Left $ Var v, Left e] st 

sem' (Right (If e s)) st = helper [Left e, Right s] st
sem' (Right (IfElse e s s')) st = helper [Left e, Right s, Right s'] st
sem' (Right (Case e xs)) st = helper (Left e : map (Right . snd) xs) st
sem' (Right (For v (lbound, hbound) stmt)) st = helper [Right (Assignment v lbound), Left hbound, Right stmt] st

helper l st = 
    case (catMaybes . map (flip sem' st)) l of
        []    -> Nothing
        l   -> Just $ unlines l

varString v = "Variable '" ++ v ++ "' "

parseProgram :: IO String -> IO (Either (PascalProgram, SymbolTable) (Either ParseError String) ) 
parseProgram inp = do
                   s <- inp
                   let p = parse program [] s
                   case p of
                       Left err -> return (Right $ Left err)
                       Right p  -> case sem p of 
                                       Left st   -> return $ Left (p, st)
                                       Right err -> return $ Right $ Right err
                   