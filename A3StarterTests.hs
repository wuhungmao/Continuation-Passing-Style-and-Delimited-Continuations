{-|
Module: A3StarterTests
Description: Starter Tests for A3
Copyright: (c) University of Toronto Mississagua
               CSC324 Principles of Programming Languages, Fall 2023
-}

module A3StarterTest

where

import Test.QuickCheck (Property, (==>), label, quickCheck)
import qualified Data.Map

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
example0 = cpsEval emptyEnv (Literal $ Error "error") id
prop_cpsEvalExample0 :: Bool
prop_cpsEvalExample0 = example0 == Error "Literal"

-- Example: apply the identity function to the number 3
-- example1 = cpsEval emptyEnv (App (Lambda ["a"] (Var "a")) [Literal $ Num 3]) id
-- prop_cpsEvalExample1 :: Bool
-- prop_cpsEvalExample1 = example1 == Num 3

-- Example: apply a function that returns 10 plus the second argument
--          to the arguments [1, 2]
-- example2 = cpsEval emptyEnv (App (Lambda ["a", "b"] (Plus (Literal $ Num 10) (Var "b")))
--                               [Literal $ Num 1, Literal $ Num 2]) id
-- prop_cpsEvalExample2 :: Bool
-- prop_cpsEvalExample2 = example2 == Num 12

-- Example: if expression
example3 = cpsEval emptyEnv (If (Equal (Literal F) (Literal F))
                             (Literal T)
                             (Literal F)) id
prop_cpsEvalExample3 :: Bool
prop_cpsEvalExample3 = example3 == T

-- Example: shift expression
example4 = (cpsEval emptyEnv
                    (Plus (Literal $ Num 2) 
                          (Shift "d" 
                              (Plus 
                                  (App (Var "d") [Literal $ Num 5]) 
                                  (App (Var "d") [Literal $ Num 10]))
                     ))
                     id)
prop_cpsEvalExample4 :: Bool
prop_cpsEvalExample4 = example4 == Num 19

-- additional tests
-- Test Plus
examplePlus = cpsEval emptyEnv (Plus (Literal $ Num 2) (Literal $ Num 3)) id
prop_cpsEvalPlus :: Bool
prop_cpsEvalPlus = examplePlus == Num 5

-- Test error propagation in Plus
examplePlusError = cpsEval emptyEnv (Plus (Literal $ Error "Literal") (Literal $ Num 3)) id
prop_cpsEvalPlusError :: Bool
prop_cpsEvalPlusError = examplePlusError == Error "Literal"

-- Test Times
exampleTimes = cpsEval emptyEnv (Times (Literal $ Num 4) (Literal $ Num 5)) id
prop_cpsEvalTimes :: Bool
prop_cpsEvalTimes = exampleTimes == Num 20

-- Test error propagation in Times
exampleTimesError = cpsEval emptyEnv (Times (Literal $ Num 4) (Literal $ Error "Literal")) id
prop_cpsEvalTimesError :: Bool
prop_cpsEvalTimesError = exampleTimesError == Error "Literal"

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


-- Valid literal
exampleLiteralNum = cpsEval emptyEnv (Literal (Num 42)) id
prop_cpsEvalLiteralNum :: Bool
prop_cpsEvalLiteralNum = exampleLiteralNum == Num 42

-- Invalid literal (assuming validLiteral rejects Closure or Error)
exampleLiteralInvalid = cpsEval emptyEnv (Literal (Error "bad")) id
prop_cpsEvalLiteralInvalid :: Bool
prop_cpsEvalLiteralInvalid = exampleLiteralInvalid == Error "Literal"

-- First of a pair
exampleFirst = cpsEval emptyEnv (First (Literal (Pair (Num 1) (Num 2)))) id
prop_cpsEvalFirst :: Bool
prop_cpsEvalFirst = exampleFirst == Num 1

-- First error propagation
exampleFirstError = cpsEval emptyEnv (First (Literal (Error "Literal"))) id
prop_cpsEvalFirstError :: Bool
prop_cpsEvalFirstError = exampleFirstError == Error "Literal"

-- First on non-pair
exampleFirstInvalid = cpsEval emptyEnv (First (Literal (Num 99))) id
prop_cpsEvalFirstInvalid :: Bool
prop_cpsEvalFirstInvalid = exampleFirstInvalid == Error "First"

-- Rest of a pair
exampleRest = cpsEval emptyEnv (Rest (Literal (Pair (Num 1) (Num 2)))) id
prop_cpsEvalRest :: Bool
prop_cpsEvalRest = exampleRest == Num 2

-- Rest error propagation
exampleRestError = cpsEval emptyEnv (Rest (Literal (Error "Literal"))) id
prop_cpsEvalRestError :: Bool
prop_cpsEvalRestError = exampleRestError == Error "Literal"

-- Rest on non-pair
exampleRestInvalid = cpsEval emptyEnv (Rest (Literal (Num 99))) id
prop_cpsEvalRestInvalid :: Bool
prop_cpsEvalRestInvalid = exampleRestInvalid == Error "Rest"

-- If with true condition
exampleIfTrue = cpsEval emptyEnv (If (Literal T) (Literal (Num 1)) (Literal (Num 2))) id
prop_cpsEvalIfTrue :: Bool
prop_cpsEvalIfTrue = exampleIfTrue == Num 1

-- If with false condition
exampleIfFalse = cpsEval emptyEnv (If (Literal F) (Literal (Num 1)) (Literal (Num 2))) id
prop_cpsEvalIfFalse :: Bool
prop_cpsEvalIfFalse = exampleIfFalse == Num 2

-- If with error condition
exampleIfError = cpsEval emptyEnv (If (Literal (Error "Literal")) (Literal (Num 1)) (Literal (Num 2))) id
prop_cpsEvalIfError :: Bool
prop_cpsEvalIfError = exampleIfError == Error "Literal"

-- Environment with variable binding
envWithX = Data.Map.fromList [("x", Num 10)]

exampleVar = cpsEval envWithX (Var "x") id
prop_cpsEvalVar :: Bool
prop_cpsEvalVar = exampleVar == Num 10

-- Variable not found
exampleVarMissing = cpsEval emptyEnv (Var "y") id
prop_cpsEvalVarMissing :: Bool
prop_cpsEvalVarMissing = exampleVarMissing == Error "Var"


-- -----------------------------------------------------------------------------
-- 1. SHIFT ABORT (No App)
-- Purpose: To test that Shift captures the entire current continuation (the full 1 + ... + 3 context)
--          and immediately aborts it by running its body (Literal 100) with the identity continuation.
-- Expression: (1 + Shift k 100) + 3
-- Expected Result: 100 (The entire surrounding calculation is discarded/aborted)
exampleShiftAbortNoApp :: Value
exampleShiftAbortNoApp = cpsEval emptyEnv 
  (Plus 
    (Literal (Num 1)) 
    (Plus 
      (Shift "k" 
        (Literal (Num 100))
      ) 
      (Literal (Num 3))
    )
  ) 
  id

prop_Shift_Abort_NoApp :: Bool
prop_Shift_Abort_NoApp = exampleShiftAbortNoApp == Num 100

-- -----------------------------------------------------------------------------
-- 2. SHIFT ABORT (With App)
-- Purpose: To test that calling the captured continuation ("d") executes the jump. 
--          The second call (d 10) is never reached because the first call (d 5) aborts the computation.
-- Expression: 2 + Shift d ( (d 5) + (d 10) )
-- Continuation captured by "d": \v -> 2 + v
-- Expected Result: 2 + 5 = 7 (The jump occurs on the first App call)
exampleShiftAppAbort :: Value
exampleShiftAppAbort = cpsEval emptyEnv 
  (Plus 
    (Literal (Num 2)) 
    (Shift "d" 
      (Plus 
        (App (Var "d") [Literal (Num 5)])
        (App (Var "d") [Literal (Num 10)])
      )
    )
  ) 
  id

prop_Shift_App_Abort :: Bool
prop_Shift_App_Abort = exampleShiftAppAbort == Num 7

-- -----------------------------------------------------------------------------
-- 3. RESET BOUNDARY (No App)
-- Purpose: To test that Reset correctly delimits the continuation captured by Shift. 
--          Shift only aborts up to the Reset, allowing the surrounding 100 + ... + 1 to continue.
-- Expression: 100 + Reset(Shift k 5) + 1
-- Execution: Reset block evaluates to 5.
-- Expected Result: 100 + 5 + 1 = 106
exampleResetNoApp :: Value
exampleResetNoApp = cpsEval emptyEnv 
    (Plus 
        (Reset 
        (Shift "k" (Literal (Num 5))) 
        ) 
        (Literal (Num 1)) 
    )
    id

prop_Reset_NoApp_Boundary :: Bool
prop_Reset_NoApp_Boundary = exampleResetNoApp == Num 6

-- -----------------------------------------------------------------------------
-- 4. RESET BOUNDARY (With App)
-- Purpose: To demonstrate the standard non-abortive use of Shift/Reset. 
--          The continuation captured by "d" is only the context inside the Reset: 1 + [].
-- Expression: 3 + Reset (1 + Shift d (d 5)) + 10
-- Continuation captured by "d": \v -> 1 + v (limited by Reset)
-- Execution: d 5 executes 1 + 5 = 6. This 6 is the result of the Reset block.
-- Outer computation: 3 + 6 + 10
-- Expected Result: 19
exampleResetAppBoundary :: Value
exampleResetAppBoundary = cpsEval emptyEnv 
  (Plus 
    (Literal (Num 3)) 
    (Plus 
      (Reset 
        (Plus 
          (Literal (Num 1)) 
          (Shift "d" 
            (App (Var "d") [Literal (Num 5)])
          )
        )
      ) 
      (Literal (Num 10)) 
    )
  ) 
  id

prop_Reset_App_Boundary :: Bool
prop_Reset_App_Boundary = exampleResetAppBoundary == Num 19

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
    quickCheck prop_cpsEvalExample0
    -- quickCheck prop_cpsEvalExample1
    -- quickCheck prop_cpsEvalExample2
    quickCheck prop_cpsEvalExample3
    -- quickCheck prop_cpsEvalExample4

    -- New CPS evaluation tests
    quickCheck prop_cpsEvalPlus
    quickCheck prop_cpsEvalPlusError
    quickCheck prop_cpsEvalTimes
    quickCheck prop_cpsEvalTimesError
    quickCheck prop_cpsEvalEqualTrue
    quickCheck prop_cpsEvalEqualFalse
    quickCheck prop_cpsEvalCons

    -- Additional CPS evaluation tests
    quickCheck prop_cpsEvalLiteralNum
    quickCheck prop_cpsEvalLiteralInvalid
    quickCheck prop_cpsEvalFirst
    quickCheck prop_cpsEvalFirstError
    quickCheck prop_cpsEvalFirstInvalid
    quickCheck prop_cpsEvalRest
    quickCheck prop_cpsEvalRestError
    quickCheck prop_cpsEvalRestInvalid
    quickCheck prop_cpsEvalIfTrue
    quickCheck prop_cpsEvalIfFalse
    quickCheck prop_cpsEvalIfError
    quickCheck prop_cpsEvalVar
    quickCheck prop_cpsEvalVarMissing

    -- Shift tests, need to add more tests here later when app is implemented
    quickCheck prop_Shift_Abort_NoApp
    quickCheck prop_Shift_App_Abort
    quickCheck prop_Reset_NoApp_Boundary
    quickCheck prop_Reset_App_Boundary

