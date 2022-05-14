import System.IO
import System.Environment
import RPtoLoop
import qualified ReducedPascal as P

-- Main logic from pascal input file to loop file
main :: IO ()
main = do
    args <- getArgs
    -- parse arguments
    case args of
        [] -> usage
        ("-o":[]) -> usage
        ("-o":outf:[]) -> usage
        ("-o":outf:inf:rest) -> run inf outf
        (inf:rest) -> run inf (takeWhile (/='.') inf ++ ".loop")
    
    where
        usage = putStrLn ("Usage :\n\t plt [-o outputfile] inputfile")
        
        -- read input file -> compile it -> if successfull write it to output file
        run inf outf = do
            inp <- P.parseProgram (readFile inf)
            case inp of 
                Left p -> writeFile outf $ show $ fst (runCalculation (programCalc p) 0)
                Right r -> putStrLn $ show r