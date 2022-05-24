module SWhile where

import qualified Loop as L
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Tree
import qualified Data.Map as Map
import Data.Maybe
import Control.Arrow


data SWhileProgram = Assignment L.Variable L.AssignmentType
                   | WhileScheme L.Variable SWhileProgram
                   | Concatenation SWhileProgram SWhileProgram
                   | ForLoop (L.Variable, L.AssignmentType) L.Variable SWhileProgram
                   | If L.Variable SWhileProgram
                   | IfNot L.Variable SWhileProgram
                   | IfElse L.Variable SWhileProgram SWhileProgram
                   | IfNotElse L.Variable SWhileProgram SWhileProgram
                   deriving (Eq)


instance Show SWhileProgram where
    show x = (drawTree . toDataTree) x ++ programFormat [] x
        where
            toDataTree (Assignment v t) = Node ("Assignment " ++ show v ++ " := " ++ show t) []
            toDataTree (WhileScheme v inner) = Node ("While " ++ show v ++ " != 0 ") [toDataTree inner]
            toDataTree (Concatenation l r) = Node "" [toDataTree l, toDataTree r] 
            toDataTree (ForLoop (v, t) v' inner) = Node ("for " ++ v ++ " := " ++ show t ++ " to " ++ v') [toDataTree inner]
            toDataTree (If v inner) = Node "If" [toDataTree inner]
            toDataTree (IfNot v inner) = Node "If not" [toDataTree inner]
            toDataTree (IfElse v inner elseinner) = Node "If-Else" [toDataTree inner, toDataTree elseinner]
            toDataTree (IfNotElse v inner elseinner) = Node "IfNot-Else" [toDataTree inner, toDataTree elseinner]

            programFormat indent (Assignment v t) = indent ++ v ++ " := " ++ L.toProgramStructure' t ++ ";\n"
            programFormat indent (WhileScheme v body) = indent ++ "while " ++ v ++ " != 0 do\n" ++ programFormat (indent ++ "    ") body ++ "\n" ++ indent ++ "end;\n"
            programFormat indent (Concatenation l r) = programFormat indent l ++ programFormat indent r
            programFormat indent (ForLoop (v, t) v' body) = indent ++ "for " ++ v ++ " := " ++ L.toProgramStructure' t ++ " to " ++ v' ++ " do\n" ++ 
                                                            programFormat (indent ++ "    ") body ++
                                                            indent ++ "end;\n"
            programFormat indent (If v inner) = indent ++ "if " ++ v ++ " do\n" ++
                                                programFormat (indent ++ "    ") inner ++
                                                indent ++ "end;\n"
            programFormat indent (IfNot v inner) = indent ++ "if not " ++ v ++ " do\n" ++
                                                programFormat (indent ++ "    ") inner ++
                                                indent ++ "end;\n"
                                                                                
            programFormat indent (IfElse v inner inner') = indent ++ "if " ++ v ++ " do\n" ++
                                                           programFormat (indent ++ "    ") (inner) ++
                                                           indent ++ "else do\n" ++
                                                           programFormat (indent ++ "    ") inner' ++
                                                           indent ++ "end;\n"
            programFormat indent (IfNotElse v inner inner') = indent ++ "if not " ++ v ++ " do\n" ++
                                                           programFormat (indent ++ "    ") (inner) ++
                                                           indent ++ "else do\n" ++
                                                           programFormat (indent ++ "    ") inner' ++
                                                           indent ++ "end;\n"
                                                           
        
                                                           
                                                           


languageDef = 
    emptyDef {
        Token.commentStart = "/*",
        Token.commentEnd   = "*/",
        Token.commentLine  = "//",
        Token.nestedComments = True,
        Token.identStart = letter,
        Token.identLetter = alphaNum,
        Token.reservedNames = [
            "while",
            "do",
            "end",
            "0",
            "1",
            "if",
            "else",
            "for",
            "to",
            "not"
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

assignment :: Parser SWhileProgram
assignment =     try (do v <- identifier; reservedOp ":="; v' <- identifier; reservedOp "+"; reserved "1"; return $ Assignment v (L.ToSucc v') )
             <|> try (do v <- identifier; reservedOp ":="; v' <- identifier; args <- parens (sepBy1 identifier (reservedOp ",")); return $ Assignment v (L.ToProgram v' args))
             <|> try (do v <- identifier; reservedOp ":="; v' <- identifier; reservedOp "-"; reserved "1"; return $ Assignment v (L.ToPred v') )
             <|> try (do v <- identifier; reservedOp ":="; reserved "0"; return $ Assignment v L.ToZero)
             <|> try (do v <- identifier; reservedOp ":="; reserved "1"; return $ Assignment v L.ToOne)
             <|>     (do v <- identifier; reservedOp ":="; v' <- identifier; return $ Assignment v (L.ToVariable v'))

forScheme :: Parser SWhileProgram
forScheme = do
    reserved "for"
    Assignment v t <- assignment
    reserved "to"
    end <- identifier
    reserved "do"
    body <- swhileProgram
    reserved "end"
    return $ ForLoop (v, t) end body

if' :: Parser SWhileProgram
if' = do
    reserved "if"
    v <- identifier 
    reserved "do"
    p <- swhileProgram
    reserved "end"
    return $ If v p
ifNot :: Parser SWhileProgram
ifNot = do
    reserved "if"
    reserved "not"
    v <- identifier 
    reserved "do"
    p <- swhileProgram
    reserved "end"
    return $ IfNot v p
ifElse :: Parser SWhileProgram
ifElse = do
    reserved "if"
    v <- identifier 
    reserved "do"
    p <- swhileProgram
    reserved "else"
    reserved "do"
    p' <- swhileProgram
    reserved "end"
    return $ IfElse v p p'

ifNotElse :: Parser SWhileProgram
ifNotElse = do
    reserved "if"
    reserved "not"
    v <- identifier 
    reserved "do"
    p <- swhileProgram
    reserved "else"
    reserved "do"
    p' <- swhileProgram
    reserved "end"
    return $ IfNotElse v p p'

whileScheme :: Parser SWhileProgram
whileScheme = do
    reserved "while"
    v <- identifier
    reservedOp "!="
    reserved "0"
    reserved "do"
    body <- swhileProgram
    reserved "end"
    return $ WhileScheme v body

swhileProgram :: Parser SWhileProgram
swhileProgram = foldl1 Concatenation <$> endBy1 (try assignment <|> try whileScheme <|> try ifNotElse <|> try ifElse <|> try ifNot <|> try if' <|> forScheme) semicolon
             
-- This function is not total which is potentially bad
-- parsing works now for the interpreter
-- Similar strategy with loop programs and holding libraries
-- inputs in i1, ..., in variables
-- outputs in o1.


interpret :: SWhileProgram -> Map.Map String SWhileProgram -> Map.Map L.Variable Int -> (Map.Map L.Variable Int)
interpret wp@(Assignment v (L.ToProgram v' args)) libs vars = 
    case (Map.lookup v' libs, and (map (isJust . flip Map.lookup vars) args)) of
        (Nothing, _)    -> error ("No program " ++ v' ++ " given as a library.")
        (Just _, False) -> error ("One of the variables is not defined")
        (Just p, True)  -> 
             case v' of
                "add"  -> f (+)
                "sub"  -> f (-)
                "mult" -> f (*)
                "div"  -> f (div)
                "mod"  -> f (mod)
                _      -> Map.insert v (fromJust $ Map.lookup "o1" (interpret p libs (Map.fromList inputs))) vars
                where
                    f g = Map.insert v ((fromJust $ lookup ("i1") inputs) `g` (fromJust $ lookup ("i2") inputs)) vars
                    inputs = flip zip (map (fromJust . flip Map.lookup vars) args) $ zipWith (\l r -> l ++ show r) (repeat "i") [1..]
                    


interpret (Assignment v t) libs vars = L.interpret (L.Assignment v t) Map.empty vars

interpret (WhileScheme v body) libs vars = 
    case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just _ -> 
            until ((==0) . fromJust . Map.lookup v) (interpret body libs) vars
interpret (ForLoop (v, t) v' body) libs vars = 
    case (Map.lookup v' vars) of
        Nothing -> error ("Variable " ++ v ++ " in for condition is not previously defined")
        _ -> 
            (until (uncurry (>) . (fromJust . Map.lookup v &&& fromJust . Map.lookup v'))) (interpret (Assignment v (L.ToSucc v)) libs. interpret body libs) (interpret (Assignment v t) libs vars )

interpret (If v body) libs vars = 
    case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just x  -> if x /= 0 then interpret body libs vars else vars

interpret (IfNot v body) libs vars = 
    case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just x  -> if x == 0 then interpret body libs vars else vars

interpret (IfElse v body ebody) libs vars = case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just x  -> if x /= 0 then interpret body libs vars else interpret ebody libs vars

interpret (IfNotElse v body ebody) libs vars = case (Map.lookup v vars) of
        Nothing -> error ("Variable " ++ v ++ " in while condition is not previously defined")
        Just x  -> if x == 0 then interpret body libs vars else interpret ebody libs vars


interpret (Concatenation l r) libs vars = (interpret r libs . interpret l libs) vars


            

compileProgram :: String -> IO SWhileProgram
compileProgram s = do
    inp <- readFile s
    let cp = parse swhileProgram [] inp
    case cp of
        Right p -> return p
        Left x  -> error (show x)