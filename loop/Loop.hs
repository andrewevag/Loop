module Loop where

import System.IO
import System.Environment
import System.Exit
import System.Path
import Data.Either
import Data.Maybe
import Data.Tree
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

-- Loop AST for Parsing
type Variable = String
data AssignmentType = ToVariable Variable
                    | ToZero
                    | ToOne
                    | ToSucc Variable
                    | ToPred Variable
                    | ToProgram String [Variable]
                    deriving (Eq, Show)

data LoopProgram = Assignment Variable AssignmentType
                 | ForLoop (Variable, AssignmentType) Variable LoopProgram
                 | Concatenation LoopProgram LoopProgram
                 deriving (Eq)

toDataTree (Assignment v t) = Node ("Assignment " ++ show v ++ " := " ++ show t) []
toDataTree (ForLoop (v, t) end program) = Node ("ForLoop " ++ show v ++ " := " ++ show t ++ "to " ++ show end) [toDataTree program]
toDataTree (Concatenation l r) = Node "" [toDataTree l, toDataTree r] 
instance Show LoopProgram where
    -- show x = (drawTree . toDataTree) x ++ "\n\n Printed Program :\n" ++ toProgramStructure [] x 
    show = toProgramStructure [] 

-- PARSER COMBINATORS FOR LOOP PROGRAMS
wsfnb :: GenParser Char st a -> GenParser Char st a
wsfnb t = do
    many space
    res <- t
    many space
    return res

semicolon :: GenParser Char st Char
semicolon = wsfnb $ char ';'

variable :: GenParser Char st Variable
variable = wsfnb $ do
    x <- letter
    xs <- many digit
    return (x:xs)

assign :: GenParser Char st String
assign = wsfnb $ do
    char ':'
    char '='
    return ":="

assignment :: GenParser Char st LoopProgram
assignment = try oneAssignment 
             <|> try zeroAssignment 
             <|> try varAssignment
             <|> try succAssignment 
             <|> try predAssignment 
             <|> programAssignment

oneAssignment :: GenParser Char st LoopProgram
oneAssignment = wsfnb $ do
    target <- variable
    assign
    wsfnb $ char '1'
    semicolon
    return $ Assignment target ToOne

zeroAssignment :: GenParser Char st LoopProgram
zeroAssignment = wsfnb $ do
    target <- variable
    assign
    wsfnb $ char '0'
    semicolon
    return $ Assignment target ToZero

varAssignment :: GenParser Char st LoopProgram
varAssignment = wsfnb $ do
    target <- variable
    assign
    op <- variable
    semicolon
    return $ Assignment target (ToVariable op)

succAssignment :: GenParser Char st LoopProgram
succAssignment = wsfnb $ do
    target <- variable
    assign
    op <- variable 
    wsfnb $ char '+'
    wsfnb $ char '1'
    semicolon
    return $ Assignment target (ToSucc op)

predAssignment :: GenParser Char st LoopProgram
predAssignment = wsfnb $ do
    target <- variable
    assign
    op <- variable 
    wsfnb $ char '-'
    wsfnb $ char '1'
    semicolon
    return $ Assignment target (ToPred op)

programAssignment :: GenParser Char st LoopProgram
programAssignment = wsfnb $ do
    target <- variable
    assign
    idbod <- many1 letter
    idtail <- many digit
    let iden = idbod ++ idtail
    wsfnb $ char '('
    vars <- variableList
    wsfnb $ char ')'
    semicolon
    return $ Assignment target (ToProgram iden vars)

-- variable list need to parse "x, y, z, w"
variableList :: GenParser Char st [Variable]
variableList = (try $ wsfnb $  do
    v <- variable
    wsfnb $ char ','
    vs <- variableList 
    return (v:vs))
    <|> (do
        v <- variable
        return [v])

number :: GenParser Char st Int
number = wsfnb $ do
    digits <- many1 digit
    return (read digits)

forSchema :: GenParser Char st LoopProgram
forSchema = wsfnb $ do
    wsfnb $ string "for"
    startVar <- variable
    assign
    c <- (try $ wsfnb $ char '0') <|> (wsfnb $ char '1')
    let ass = case c of 
                  '0' -> ToZero
                  _   -> ToOne
    wsfnb $ string "to"
    endVar <- variable
    wsfnb $ string "do"
    p <- loopProgram
    wsfnb $ string "done"
    return $ ForLoop (startVar, ass) endVar p

loopProgram :: GenParser Char st LoopProgram
loopProgram = wsfnb $ do
    p:ps <- many1 (try assignment <|> forSchema)
    
    return $ foldl Concatenation p ps


    
            
-- The actuall interpretation of a compiled LoopProgram
-- 
-- With the program we have a map<libraryProgram, Syntax of the program> 
-- and the result of the computation is map<string, Int> the final values
-- of all variables after calculation
interpret :: LoopProgram -> Map.Map String LoopProgram -> Map.Map Variable Int -> (Map.Map Variable Int)
-- Change the value of v to the new one according to the assignment
-- if a function call is in the result to add 
-- ---> Stop calculation
-- ---> Interpret the library function with arguments those of the function call
interpret a@(Assignment v t) libs prevMap = 
    case t of
        ToVariable v' -> valInsert v v' id prevMap
        ToZero        -> Map.insert v 0 prevMap
        ToOne         -> Map.insert v 1 prevMap
        ToSucc v'     -> valInsert v v' (+1) prevMap
        ToPred v'     -> valInsert v v' (\x -> if x == 0 then 0 else x - 1) prevMap
        ToProgram iden vars -> Map.insert v (fromJust $ Map.lookup "o1" (independentProgram iden vars)) prevMap
    where
        independentProgram :: String -> [Variable] -> Map.Map Variable Int
        independentProgram iden vars =
            let
                parsedP = Map.lookup iden libs
                func :: Map.Map Variable Int -> (Int, Variable) -> Map.Map Variable Int
                func prev (index, curr) =
                    Map.insert ("i" ++ show index) (fromJust $ Map.lookup curr prevMap) prev 
            in
                case isNothing parsedP of
                    True -> error ("no " ++ show iden ++ " program in the library\n all are:\n" ++ show (Map.keys libs))
                    _    -> interpret (fromJust parsedP) libs (foldl func Map.empty (zip [1..] vars))
        
        valInsert :: Variable -> Variable -> (Int -> Int) -> Map.Map Variable Int -> Map.Map Variable Int
        valInsert v v' f prevMap = 
                if v' `Map.notMember` prevMap then error ("unknwon variable " ++ v' ++ " in " ++ show a) else Map.insert v (f $ fromJust $ Map.lookup v' prevMap) prevMap

-- iterate the interpretation of the body from startval to endval
interpret a@(ForLoop (startvar, startval) endvar innerProgram) libs prevMap =
    let 
        m' = interpret (Assignment startvar startval) libs prevMap
        
        loop :: Variable -> Variable -> Map.Map Variable Int -> Map.Map Variable Int
        loop start end currMap =
            let 
                res1 = Map.lookup start currMap
                res2 = Map.lookup end currMap
            in
                case (res1,res2) of
                    (Nothing, _)     -> error ("you gave me a variable in a loop that does not exist :" ++ show start)
                    (_, Nothing) -> error ("you gave me a variable in a loop that does not exist :" ++ show end)
                    _             -> if (fromJust res1) > (fromJust res2) 
                                     then currMap 
                                     else (loop start end . interpret (Assignment start (ToSucc start)) libs . interpret innerProgram libs ) currMap
            
    in
        loop startvar endvar m'

-- Interpret left then interpret right           
interpret a@(Concatenation l r) libs prevMap = (interpret r libs . interpret l libs) prevMap



-- New stuff for the compilation of a program targeting Loop so it can be printed
-- Directly into loop code from the AST of LoopProgram

toProgramStructure' (ToVariable v) = v
toProgramStructure' (ToZero) = "0"
toProgramStructure' (ToOne) = "1"
toProgramStructure' (ToSucc v) = v ++ " + 1" 
toProgramStructure' (ToPred v) = v ++ " - 1"
toProgramStructure' (ToProgram p vars) = p ++ "(" ++ intercalate ", " vars ++ ")"

toProgramStructure indent (Assignment v t) = indent ++ v ++ " := " ++ toProgramStructure' t ++ ";\n"
toProgramStructure indent (ForLoop (v, t) end inner) = 
    indent ++ "for " ++ v ++ " := " ++ toProgramStructure' t  ++ " to " ++ end ++ " do\n" ++
    (toProgramStructure ("    " ++ indent) inner) ++ 
    indent ++ "done\n"
toProgramStructure indent (Concatenation l r) = 
    (toProgramStructure indent l) ++ 
    (toProgramStructure indent r)
