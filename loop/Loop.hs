import System.IO
import System.Environment
import System.Exit
import System.Path
import Data.Either
import Data.Maybe
import Data.Tree
import Control.Monad
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    inp <- readFile $ head args
    -- ok so now need to parse --lib 
    libs <- case tail args of
        ("--lib":xs) -> return xs
        _            -> return []
    -- loopProgram ex add(x, y) maybe inputs will be i1, i2, i3, i4 and output will be o1 o2 ...
    
    -- libs' are the identifier names for the lib operand in interpret
    -- remove .loop
    let libs' = map  (reverse . drop 5 . reverse . keepName) libs
    inputsLib <- mapM readFile libs
    --parse the libraries
    let parsedLib = map (parse loopProgram "unknown" . removeComments) inputsLib
    
    putStrLn "Parsed Libraries : " 
    mapM_ (putStrLn. show) (rights parsedLib)
    case all isRight parsedLib of
        False -> do 
                 putStrLn "failed to parse lib"
                 exitFailure
        True ->  do
                 let p =  (parse loopProgram "unknown" . removeComments) inp
                 let lib = foldl (\prevMap (name, prog) -> Map.insert name prog prevMap) Map.empty (zip libs' (rights parsedLib))
                 putStrLn "Parse Tree :"
                 putStrLn $ show p
                 case p of 
                     Left err -> do {putStrLn (show err); return ()}
                     Right p -> do
                                 let resMap = interpret p lib Map.empty
                                 putStrLn "Results : "
                                 mapM_ (\(a, b) -> putStrLn (show a ++ "=" ++ show b)) (Map.assocs resMap)
                                 return ()

keepName :: String -> String 
keepName l = if '/' `elem` l then keepName $ tail $ dropWhile (/='/') l else l

removeComments :: String -> String
removeComments = unlines . filter (not . f) . lines
    where
        f ('-':'-':xs) = True
        f _            = False

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
    show = drawTree . toDataTree

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
    p <- try assignment <|> forSchema 
    ps <- many loopProgram
    return $ foldl Concatenation p ps


interpret :: LoopProgram -> Map.Map String LoopProgram -> Map.Map Variable Int -> (Map.Map Variable Int)
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
            
interpret a@(Concatenation l r) libs prevMap = (interpret r libs . interpret l libs) prevMap
