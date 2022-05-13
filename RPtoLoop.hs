module RPtoLoop where

import qualified ReducedPascal as P
import qualified Loop as L
import qualified Data.Map as Map


-- Operators to help not writing a lot of stuff

-- ASSIGNMENT TO FUNCTION CALL
(<%) :: L.Variable -> (String, [L.Variable]) -> L.LoopProgram
var <% (fname, args) = L.Assignment var (L.ToProgram fname args)

-- ASSIGNMENT TO SOMETHING
(<==) :: L.Variable -> L.AssignmentType -> L.LoopProgram
var <== t = L.Assignment var t

-- INFIX CONCATENATION
(=>>) :: L.LoopProgram -> L.LoopProgram -> L.LoopProgram
l =>> r = L.Concatenation l r
infixl 0 =>>

-- GET THE NEW VARIABLE OF AN INTEGER
decode :: Show a => a -> String
decode x = ("x" ++ show x)

-- END OF OPERATORS

-- THIS DEFINES A DATATYPE FOR CALCULATION
-- CALCULATIONS CAN BE COMBINED AND APPLIED THUS 
-- THE APPLICATIVE AND MONAD DEFINITIONS

-- A Pascal Program will be defined as a UniqueCalculation of a LoopProgram
newtype UniqueCaclulation a = Calculation {
    runCalculation :: Integer -> (a, Integer)
} 

instance Functor (UniqueCaclulation) where
    fmap f (Calculation g) = Calculation $ \i ->
        let
            (res, next) = g i
        in
            (f res, next)

instance Applicative (UniqueCaclulation) where
    -- pure :: a -> f a
    pure x = Calculation $ \i -> (x, i)
    -- Calculation b (a -> c) === b -> Integer -> ((a -> c), Integer)
    -- Calculation b c        === b -> Integer -> (c, Integer)
    (Calculation f) <*> (Calculation p) = Calculation $ \i ->
        let
            (res, next) = f i 
            (res', next') = p next
        in
            (res res', next')

instance Monad (UniqueCaclulation) where
    return = pure
    -- (Integer -> (a, Integer)) -> (a -> (Integer -> (b, Integer))) -> (Integer -> (b, Integer))
    (Calculation p) >>= (f) = 
        Calculation $ \i ->
            let
                (res, next) = p i
                Calculation g = f res
            in
                g next

-- Grab an Integer for creating a new unique variable and change the state to the next Integer
next :: UniqueCaclulation Integer
next = Calculation $ \i -> (i, i+1)

-- Calculate the increment of a calculation
incCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable])  -> UniqueCaclulation (L.LoopProgram, [L.Variable]) 
incCalc p = do
            (res, rest) <- p
            let f = head rest
            return $ (res =>> f <== L.ToSucc (f), [f])

-- Calculate 0
calcZero :: UniqueCaclulation (L.LoopProgram, [L.Variable]) 
calcZero = do
    var <- decode <$> next
    return (var <== L.ToZero, [var])

-- Calculate a constant n
constantCalc :: Integer -> UniqueCaclulation (L.LoopProgram, [L.Variable])
constantCalc 0 = calcZero
constantCalc n = incCalc (constantCalc (n-1))

-- Calculate a series of calculations
calculateAll :: [UniqueCaclulation (L.LoopProgram, [L.Variable])] -> UniqueCaclulation (L.LoopProgram, [L.Variable])
calculateAll [x] = x
calculateAll (x:xs) = do
    (res, vars) <- x
    (res', vars') <- calculateAll xs
    return (res =>> res', vars ++ vars')

-- Pass to a function results of a list of computations
funCallCalc :: String -> [UniqueCaclulation (L.LoopProgram, [L.Variable])] -> UniqueCaclulation (L.LoopProgram, [L.Variable])
funCallCalc s vs = do
    (res, resvars) <- calculateAll vs
    var <- decode <$> next
    return (res =>> var <% (s, resvars), [var])

-- Calculate a variable
variableCalc :: P.Variable -> UniqueCaclulation (L.LoopProgram, [L.Variable])
variableCalc (P.Variable v) = do
    return (v <== L.ToVariable v, [v])
-- missing definition for arrays

-- Caclualte if two calculations produce equal results
equalsCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
equalsCalc l r = funCallCalc "equals" [l, r]

-- Caclualte if two calculations produce different results
differentCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
differentCalc l r = do 
    (res, vars) <- equalsCalc l r
    var <- decode <$> next
    return (res =>> var <% ("ifnzero", vars), [var])

-- Caclualte if the first calculation is smaller than the second
lessCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
lessCalc l r = funCallCalc "less" [l, r]

-- Caclualte if the first calculation is smaller or equals to the second
lessEqCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
lessEqCalc l r = do
    (res, rvar) <- l
    (res', rvar') <- r
    let var = head rvar
    let var' = head rvar'
    var1 <- decode <$> next
    var2 <- decode <$> next
    var3 <- decode <$> next
    return (res =>> res' =>> var1 <% ("less", [var, var']) =>> var2 <% ("equals", [var, var']) =>> var3 <% ("or", [var1, var2]), [var3])

-- Caclualte if the first calculation is greater than the second
greaterCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
greaterCalc l r = funCallCalc "greater" [l, r]

-- Caclualte if the first calculation is greater or equals to the second
greaterEqCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
greaterEqCalc l r = do
    (res, rvar) <- l
    (res', rvar') <- r
    let var = head rvar
    let var' = head rvar'
    var1 <- decode <$> next
    var2 <- decode <$> next
    var3 <- decode <$> next
    return (res =>> res' =>> var1 <% ("greater", [var, var']) =>> var2 <% ("equals", [var, var']) =>> var3 <% ("or", [var1, var2]), [var3])

-- Plus of two calculations
plusCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
plusCalc l r = funCallCalc "add" [l, r]

-- Minus of two calculations ...
minusCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
minusCalc l r = funCallCalc "sub" [l, r]

multCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
multCalc l r = funCallCalc "mult" [l, r]

divCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
divCalc l r = funCallCalc "div" [l, r]

modCalc :: UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable]) -> UniqueCaclulation (L.LoopProgram, [L.Variable])
modCalc l r = funCallCalc "mod" [l, r]


-- Do it from the top now
expressionCalc :: P.Expression -> UniqueCaclulation (L.LoopProgram, [L.Variable])
expressionCalc (P.Equals l r) = equalsCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Different l r) = differentCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Less l r) = lessCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.LessEq l r) = lessEqCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Greater l r) = greaterCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.GreaterEq l r) = greaterEqCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Plus l r) = plusCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Minus l r) = minusCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Mult l r) = multCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Div l r) = divCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Mod l r) =  modCalc (expressionCalc l) (expressionCalc r)
expressionCalc (P.Constant i) = constantCalc i
expressionCalc (P.Var v) = variableCalc v

{-
  data Statement = Block [Statement]
               | Assignment Variable Expression
               | If Expression Statement
               | IfElse Expression Statement Statement
               | Case Expression [(Integer, Statement)]
               | For Variable (Expression, Expression) Statement
               -- in the for loop (from-lowerBound, to-UpperBound)
               deriving (Eq, Show)  
-}
statementCalc :: P.Statement -> UniqueCaclulation L.LoopProgram
statementCalc (P.Block [stmt]) = statementCalc stmt
statementCalc (P.Block (st:sts)) = do
    res <- statementCalc st 
    res' <- statementCalc (P.Block sts)
    return (res =>> res')

statementCalc (P.Assignment (P.Variable v) e) = do
    (res', var') <- expressionCalc e
    let var = head var'
    return (res' =>> v <== L.ToVariable var)

statementCalc (P.If e stmt) = do
    (res, vars) <- funCallCalc "ifnzero" [expressionCalc e]
    let var = head vars
    (upperBound, upvar) <- constantCalc 1
    let uvar = head upvar
    nvar <- decode <$> next
    res' <- statementCalc stmt
    return (res =>> upperBound =>> L.ForLoop (nvar, L.ToVariable uvar) var res')

statementCalc (P.IfElse e stmt stmt')
    (res, vars) <- funCallCalc "ifnzero" [expressionCalc e]
    let var = head vars
    rstmt <- statementCalc stmt
    rstmte <- statementCalc stmt'
    
