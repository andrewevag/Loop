module Main where

import While
import System.Environment
import System.Exit
import Control.Monad
import qualified Data.Map as Map
-- the brains of interpreting a loop program

-- main :: IO ()
main = do
    args <- getArgs
    (fp, flibs) <- case args of 
            [h]               -> return (h, [])
            (h:"--lib":rest)  -> return (h, rest)
            _                 -> die "Usage is while <inputfile> [--libs <program1> ...]"
    p <- compileProgram fp
    libs <- zip flibs <$> mapM compileProgram flibs
    -- print the program
    print fp ; print p
    -- print libraries
    mapM (\(l,r) -> do {print l; print r}) libs
    let libs' = map (\(x,y) -> ((reverse . drop (length ".while") . reverse . until ('/' `notElem`) tail) x, y)) libs
    let resMap = interpret p (Map.fromList libs') Map.empty   
    mapM (\(l, r) -> do {putStr (show l); putStr " = "; putStrLn (show r)}) (Map.toList resMap)