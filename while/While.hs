module While where

import qualified Loop as L
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Tree
import qualified Data.Map as Map
import Data.Maybe

data WhileProgram = Assignment L.Variable L.AssignmentType
                  | WhileScheme L.Variable WhileProgram
                  | Concatenation WhileProgram WhileProgram
                  deriving (Eq)


instance Show WhileProgram where
    show x = (drawTree . toDataTree) x ++ programFormat [] x
        where
            toDataTree (Assignment v t) = Node ("Assignment " ++ show v ++ " := " ++ show t) []
            toDataTree (WhileScheme v inner) = Node ("While " ++ show v ++ " != 0 ") [toDataTree inner]
            toDataTree (Concatenation l r) = Node "" [toDataTree l, toDataTree r] 

            programFormat indent (Assignment v t) = indent ++ v ++ " := " ++ L.toProgramStructure' t
            programFormat indent (WhileScheme v body) = indent ++ "while " ++ v ++ " != 0 do\n" ++ programFormat (indent ++ "    ") body ++ "\n" ++ indent ++ "end"
            programFormat indent (Concatenation l r) = indent ++ programFormat indent l ++ ";\n" ++ indent ++ programFormat indent r

languageDef = 
    emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd   = "*/",
        Token.nestedComments = True,
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = [
            "while",
            "do",
            "end",
            "0",
            "1"
        ],
        Token.reservedOpNames = [
            ":=",
            "+",
            "-",
            "!=",
            ";",
            ","
        ]
    }

lexer = Token.makeTokenParser languageDef

-- All the individual parsers for parsing lexemes
identifier  = Token.identifier      lexer
reserved    = Token.reserved        lexer
reservedOp  = Token.reservedOp      lexer
parens      = Token.parens          lexer
natural     = Token.natural         lexer
semicolon   = Token.semi            lexer
whiteSpace  = Token.whiteSpace      lexer

assignment :: Parser WhileProgram
assignment =     try (do v <- identifier; reservedOp ":="; v' <- identifier; reservedOp "+"; reserved "1"; return $ Assignment v (L.ToSucc v') )
             <|> try (do v <- identifier; reservedOp ":="; v' <- identifier; args <- parens (sepBy1 identifier (reservedOp ",")); return $ Assignment v (L.ToProgram v' args))
             <|> try (do v <- identifier; reservedOp ":="; v' <- identifier; reservedOp "-"; reserved "1"; return $ Assignment v (L.ToPred v') )
             <|> try (do v <- identifier; reservedOp ":="; reserved "0"; return $ Assignment v L.ToZero)
             <|> try (do v <- identifier; reservedOp ":="; reserved "1"; return $ Assignment v L.ToOne)
             <|>     (do v <- identifier; reservedOp ":="; v' <- identifier; return $ Assignment v (L.ToVariable v'))

whileScheme :: Parser WhileProgram
whileScheme = do
    reserved "while"
    v <- identifier
    reservedOp "!="
    reserved "0"
    reserved "do"
    body <- whileProgram
    reserved "end"
    return $ WhileScheme v body

whileProgram :: Parser WhileProgram
whileProgram = foldl1 Concatenation <$> endBy1 (try assignment <|> whileScheme) semicolon
             
-- This function is not total which is potentially bad
-- parsing works now for the interpreter
-- Similar strategy with loop programs and holding libraries
-- inputs in i1, ..., in variables
-- outputs in o1.
interpret :: WhileProgram -> Map.Map String WhileProgram -> Map.Map L.Variable Int -> (Map.Map L.Variable Int)
interpret wp@(Assignment v (L.ToProgram v' args)) libs vars = 
    case (Map.lookup v' libs, and (map (isJust . flip Map.lookup vars) args)) of
        (Nothing, _)    -> error ("No program " ++ v' ++ " given as a library.")
        (Just _, False) -> error ("One of the variables is not defined")
        (Just p, True)  -> Map.insert v (fromJust $ Map.lookup "o1" (interpret p libs (Map.fromList inputs))) vars
            where
                inputs = flip zip (map (fromJust . flip Map.lookup vars) args) $ zipWith (\l r -> l ++ show r) (repeat "i") [1..] 


interpret (Assignment v t) libs vars = L.interpret (L.Assignment v t) Map.empty vars

interpret (WhileScheme v body) libs vars = 
    case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just _ -> 
            until ((==0) . fromJust . Map.lookup v) (interpret body libs) vars

interpret (Concatenation l r) libs vars = (interpret r libs . interpret l libs) vars


            

compileProgram :: String -> IO WhileProgram
compileProgram s = do
    inp <- readFile s
    let cp = parse whileProgram [] inp
    case cp of
        Right p -> return p
        Left x  -> error (show x)