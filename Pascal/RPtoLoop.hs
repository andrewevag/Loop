module RPtoLoop where

import ReducedPascal
import Loop

-- Can't think of monadic design so we will go back to how we were doing things
-- the types of PascalPrograms -> Loop === PascalPogram -> Int -> (Loop Program, Int) 
-- 

(==>) :: Integer -> (String -> a) -> (a, Integer)
x ==> f = (f ("x" ++ read x), x+1) 



-- expression :: ReducedPascal.Expression -> Integer -> (LoopProgram, [String], Int)
expression (Constant x) prev = prev ==> (\var -> foldl (\prev curr -> Concatenation prev (Assignment var ToSucc)) (Assignment var ToZero) [1..prev])


