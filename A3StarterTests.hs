{-|
Module: A3StarterTests
Description: Starter Tests for A3
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2023
-}

module A3StarterTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)

import A3 (cpsFactorial, cpsFibonacci, cpsLength, cpsMap,
           cpsMergeSort, cpsSplit, cpsMerge, cpsEval)
import A3Types (Env, emptyEnv, Value(..), Expr(..))

-- | Warmup Task tests

-- calling cpsFactorial with an identity continuation should result in
-- the same behaviour as the usual factorial function
prop_testFactorial_id :: Bool
prop_testFactorial_id = (cpsFactorial 3 id) == 6

-- calling *factorial* with an actual continuation function should
-- result in that conctinuation applied after factorial is computed
prop_testFactorial_add1 :: Bool
prop_testFactorial_add1 = (cpsFactorial 3 (1 +)) == 7

-- a more general test to ensure that the continuation function
-- is applied correctly.
prop_testFactorial_cont :: Int -> Property
prop_testFactorial_cont n = (n >= 0 && n <= 20) ==> (cpsFactorial n (1 +)) == (1 + cpsFactorial n id)

-- testing the correctness of cpsFibonnaci function
prop_testFibonacci_id :: Bool
prop_testFibonacci_id = (cpsFibonacci 6 id) == 8

-- testing that the continuations are applied correctly
prop_testFibonacci_cont :: Int -> Property
prop_testFibonacci_cont n = (n >= 0 && n <= 20) ==> (cpsFibonacci n (+ 1)) == (1 + cpsFibonacci n id)

-- testing the correctness of cpsLength, cpsMap
prop_cpsLength :: Bool
prop_cpsLength = (cpsLength [1, 2, 3] id) == 3
prop_cpsLength_cont :: Bool
prop_cpsLength_cont = (cpsLength [1, 2, 3] (1 +)) == 4

prop_cpsMap :: Bool
prop_cpsMap = (cpsMap (2 *) [1, 2, 3, 4, 5] id) == [2, 4, 6, 8, 10]
prop_cpsMap_cont :: Bool
prop_cpsMap_cont = (cpsMap (2 *) [1, 2, 3, 4, 5] sum) == 30

prop_cpsMergeSort :: Bool
prop_cpsMergeSort = (cpsMergeSort [1, 2, 4, 3] id) == [1, 2, 3, 4]

-- | cpsEval tests

-- Example: invalid literal
-- example0 = cpsEval emptyEnv (Literal $ Error "error") id

-- Example: apply the identity function to the number 3
-- example1 = cpsEval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3]) id

-- Example: apply a function that returns 10 plus the second argument
--          to the arguments [1, 2]
-- example2 = cpsEval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
--                               [Literal $ Num 1, Literal $ Num 2]) id
-- Example: if expression
-- example3 = cpsEval emptyEnv (If (Equal (Literal F) (Literal F))
--                              (Literal T)
--                              (Literal F)) id
-- Example: shift expression
-- example4 = (cpsEval emptyEnv
--                     (Plus (Literal $ Num 2) 
--                           (Shift "d" 
--                               (Plus 
--                                   (App (Var "d") [Literal $ Num 5]) 
--                                   (App (Var "d") [Literal $ Num 10]))
--                      ))
--                      id)


-- additional tests
-- Test Plus
examplePlus = cpsEval emptyEnv (Plus (Literal $ Num 2) (Literal $ Num 3)) id
prop_cpsEvalPlus :: Bool
prop_cpsEvalPlus = examplePlus == Num 5

-- Test error propagation in Plus
examplePlusError = cpsEval emptyEnv (Plus (Literal $ Error "bad") (Literal $ Num 3)) id
prop_cpsEvalPlusError :: Bool
prop_cpsEvalPlusError = examplePlusError == Error "bad"

-- Test Times
exampleTimes = cpsEval emptyEnv (Times (Literal $ Num 4) (Literal $ Num 5)) id
prop_cpsEvalTimes :: Bool
prop_cpsEvalTimes = exampleTimes == Num 20

-- Test error propagation in Times
exampleTimesError = cpsEval emptyEnv (Times (Literal $ Num 4) (Literal $ Error "oops")) id
prop_cpsEvalTimesError :: Bool
prop_cpsEvalTimesError = exampleTimesError == Error "oops"

-- Test Equal (true case)
exampleEqualTrue = cpsEval emptyEnv (Equal (Literal $ Num 7) (Literal $ Num 7)) id
prop_cpsEvalEqualTrue :: Bool
prop_cpsEvalEqualTrue = exampleEqualTrue == T

-- Test Equal (false case)
exampleEqualFalse = cpsEval emptyEnv (Equal (Literal $ Num 7) (Literal $ Num 8)) id
prop_cpsEvalEqualFalse :: Bool
prop_cpsEvalEqualFalse = exampleEqualFalse == F

-- Test Cons
exampleCons = cpsEval emptyEnv (Cons (Literal $ Num 1) (Literal $ Num 2)) id
prop_cpsEvalCons :: Bool
prop_cpsEvalCons = exampleCons == Pair (Num 1) (Num 2)


-- prop_cpsEvalExample0 :: Bool
-- prop_cpsEvalExample0 = example0 == Error "Literal"
-- prop_cpsEvalExample1 :: Bool
-- prop_cpsEvalExample1 = example1 == Num 3
-- prop_cpsEvalExample2 :: Bool
-- prop_cpsEvalExample2 = example2 == Num 12
-- prop_cpsEvalExample3 :: Bool
-- prop_cpsEvalExample3 = example3 == T
-- prop_cpsEvalExample4 :: Bool
-- prop_cpsEvalExample4 = example4 == Num 19


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

-- | This main function runs the quickcheck tests.
-- This gets executed when you compile and run this program. We'll talk about
-- "do" notation much later in the course, but for now if you want to add your
-- own tests, just define them above, and add a new `quickcheck` line here.
--
main :: IO ()
main = do
    quickCheck prop_testFactorial_id
    quickCheck prop_testFactorial_add1
    quickCheck prop_testFactorial_cont
    quickCheck prop_testFibonacci_id
    quickCheck prop_testFibonacci_cont
    quickCheck prop_cpsLength
    quickCheck prop_cpsLength_cont
    quickCheck prop_cpsMap
    quickCheck prop_cpsMap_cont
    quickCheck prop_cpsMergeSort
    -- quickCheck prop_cpsEvalExample1
    -- quickCheck prop_cpsEvalExample2
    -- quickCheck prop_cpsEvalExample3
    -- quickCheck prop_cpsEvalExample4

    -- New CPS evaluation tests
    quickCheck prop_cpsEvalPlus
    quickCheck prop_cpsEvalTimes
    quickCheck prop_cpsEvalEqualTrue
    quickCheck prop_cpsEvalEqualFalse
    quickCheck prop_cpsEvalCons
    quickCheck prop_cpsEvalPlusError
    quickCheck prop_cpsEvalTimesError

