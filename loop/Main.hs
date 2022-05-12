import Loop
import System.IO
import System.Environment
import System.Exit
import System.Path
import Data.Either
import Data.Maybe
import Data.Tree
import Control.Monad
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

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
    mapM_ (putStrLn) (zipWith (\x y -> y ++ "\n" ++ show x) (parsedLib) libs)
    case all isRight parsedLib of
        False -> do
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

