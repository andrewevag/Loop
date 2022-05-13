module RPtoLoop where

import qualified ReducedPascal as P
import qualified Loop as L
import qualified Data.Map as Map

(==>) :: Integer -> (String -> a) -> (a, Integer)
x ==> f = (f ("x" ++ show x), x+1) 
infixl 1 ==>

decode x = ("x" ++ show x)

-- Operators to help not writing a lot of stuff
-- to function call assignment
(<%) :: L.Variable -> (String, [L.Variable]) -> L.LoopProgram
var <% (fname, args) = L.Assignment var (L.ToProgram fname args)

(<==) :: L.Variable -> L.AssignmentType -> L.LoopProgram
var <== t = L.Assignment var t

-- Infix Concatenation
(=>>) :: L.LoopProgram -> L.LoopProgram -> L.LoopProgram
l =>> r = L.Concatenation l r
infixl 0 =>>

-- Στην πράξη κάνε lift το όνομα της επόμενης σε σειρά μεταβλητής να χρησιμοποιηθεί.
-- This will act as a chain
(==>>) :: Integer -> [P.Expression] -> ([(L.LoopProgram, L.Variable)], Integer)
i ==>> pis = (reverse res, nnn)
    where 
        (res, nnn) = foldl f ([], i) pis
        f (prev, next) curr = 
            let
                (res, next') = expression curr next
            in
                (res : prev, next')


expression :: P.Expression -> Integer -> ((L.LoopProgram, L.Variable), Integer)
expression (P.Constant x) prev = prev ==> 
    (\var -> (foldl (\prev curr ->  prev =>> (var <== (L.ToSucc var))) (var <== L.ToZero) [1..x], var))

expression (P.Var (P.Variable v)) prev = 
    ((v <== (L.ToVariable v), v), prev)

expression (P.Var (P.ArrayIndexedAt v expr)) prev = undefined

expression (P.Equals l r) prev = binApply l r "equals" prev 

expression (P.Different l r) prev = 
    let
        ([(res, resvar1)], next) = prev ==>> [P.Equals l r]
    in
        next ==> \var -> (res =>> var <% ("ifzero", [resvar1]), var)
    
expression (P.Less l r) prev = binApply l r "less" prev

expression (P.LessEq l r) prev = 
    let
        ([(res1, resvar1), (res2, resvar2)], next) = prev ==>> [l, r]
        (nvar1, next') = next ==> id
        (nvar2, next'') = next' ==> id
        (nvar3, next''') = next'' ==> id
        prg = res1 =>> res2 =>> nvar1 <% ("less", [resvar1, resvar2]) =>> nvar2 <% ("equals", [resvar1, resvar2]) =>> nvar3 <% ("or", [nvar1, nvar2])
    in
        ((prg, nvar2), next''')


expression (P.Greater l r) prev = binApply l r "greater" prev

expression (P.GreaterEq l r) prev = 
    let
        ([(res1, resvar1), (res2, resvar2)], next) = prev ==>> [l, r]
        (nvar1, next') = next ==> id
        (nvar2, next'') = next' ==> id
        (nvar3, next''') = next'' ==> id
        prg = res1 =>> res2 =>> nvar1 <% ("greater", [resvar1, resvar2]) =>> nvar2 <% ("equals", [resvar1, resvar2]) =>> nvar3 <% ("or", [nvar1, nvar2])
    in
        ((prg, nvar2), next''')

expression (P.Plus l r) prev = binApply l r "add" prev
expression (P.Mult l r) prev = binApply l r "mult" prev
expression (P.Minus l r) prev = binApply l r "sub" prev
expression (P.Div l r) prev = binApply l r "div" prev
expression (P.Mod l r) prev = binApply l r "mod" prev


binApply l r f prev = 
    let
        ([(res1, resvar1), (res2, resvar2)], next) = prev ==>> [l, r]
    in
        next ==> \var -> (res1 =>> res2 =>> var <% (f, [resvar1, resvar2]), var) 

-- The expressions are compiled successfully now we need to define 
-- what to do with a complete pascal program

-- The pascal program has some variables at the front that need to be changed into a dictionary 
-- of lookups String 
-- Thus far we ignore in expressions that variables can cause a problem

-- uparxei ki allos tropos
-- kane generate thn loop me ta onomata idia kai alla3e ta sto telos
-- auto 9a ginei mallon 

statement :: P.Statement -> Integer -> (L.LoopProgram, Integer)
statement (Block)