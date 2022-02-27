{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

import Ex04

main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\n\nExercise 04 Tests (50 BB Points)\n"
            [ testGroup "\nDeclaration" [ testCase declaration (1+1 @?= 2) ]
            , insertTests
            , lookupTests
            , statsTests
            ]
        ]


-- tests for Part 1 (insert) -----------------------------------------------------

type IntTree = BinTree Int Int -- binary tree with integer keys and data

insertTests :: TF.Test
insertTests
 = testGroup "\nPart 1 - Tree Insert (17 marks)\n"
    [ testCase "1 -> 100 into Empty [1 mark]"
      ( ins 1 100 Empty @?= Leaf 1 100 )
    , testCase "3->300 into Leaf 5 500 [2 marks]"
      ( ins 3 300 (Leaf 5 500) @?= Branch (Leaf 3 300) Empty 5 500 )
    , testCase "5->5000 into Leaf 5 500 [2 marks]"
      ( ins 5 5000 (Leaf 5 500) @?= Leaf 5 5000 )
    , testCase "7->700 into Leaf 5 500 [2 marks]"
      ( ins 7 700 (Leaf 5 500) @?= Branch Empty (Leaf 7 700) 5 500 )
    , testCase "3->300 into Branch Empty (Leaf 7 700) 5 500 [2 marks]"
      ( ins 3 300 (Branch Empty (Leaf 7 700) 5 500)
       @?= Branch (Leaf 3 300) (Leaf 7 700) 5 500 )
    , testCase "7->700 into Branch (Leaf 3 300) Empty 5 500 [2 marks]"
      ( ins 7 700 (Branch (Leaf 3 300) Empty 5 500)
        @?= Branch (Leaf 3 300) (Leaf 7 700) 5 500 )
    , testCase "4->400 into Branch (Leaf 3 300) (Leaf 7 700) 5 500 [2 marks]"
       ( ins 4 400 (Branch (Leaf 3 300) (Leaf 7 700) 5 500) @?=
           Branch (Branch Empty (Leaf 4 400) 3 300) (Leaf 7 700) 5 500 )
    , testCase "6->600 into Branch (Leaf 3 300) (Leaf 7 700) 5 500 [2 marks]"
       ( ins 6 600 (Branch (Leaf 3 300) (Leaf 7 700) 5 500) @?=
           Branch (Leaf 3 300) (Branch (Leaf 6 600) Empty 7 700) 5 500 )
    , testCase "7->49 into Branch (Leaf 3 300) (Leaf 7 700) 5 500 [2 marks]"
       ( ins 7 49 (Branch (Leaf 3 300) (Leaf 7 700) 5 500) @?=
           Branch (Leaf 3 300) (Leaf 7 49) 5 500 )
    ]


-- Tests for Part 2 (lookup) -----------------------------------------------------

empty :: IntTree
empty = Empty -- needed to avoid explicit type signatures below

lookupTests :: TF.Test
lookupTests
 = testGroup "\nPart 2 - Tree Lookup (17 marks)\n"
    [ testCase "3 in Empty ? (Maybe)[1 mark]"
        (lkp empty 3 @?= Nothing )
    , testCase "3 in Empty ? ([])[1 mark]"
        (lkp empty 3 @?= [] )
    , testCase "3 in Leaf 5 500 (Maybe))[1 mark]"
        (lkp (Leaf 5 500) 3 @?= Nothing )
    , testCase "3 in Leaf 5 500 ([]))[1 mark]"
        (lkp (Leaf 5 500) 3 @?= [] )
    , testCase "3 in Leaf 3 300 (Maybe))[1 mark]"
        (lkp (Leaf 3 300) 3 @?= Just 300 )
    , testCase "3 in Leaf 3 300 ([]))[2 marks]"
        (lkp (Leaf 3 300) 3 @?= [300] )
    , testCase "3 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 ([]))[1 mark]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 3 @?= [300] )
    , testCase "7 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 (Maybe))[2 marks]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 7 @?= Just 700 )
    , testCase "5 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 ([]))[1 mark]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 5 @?= [500] )
    , testCase "1 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 (Maybe))[1 mark]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 1 @?= Nothing )
    , testCase "4 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 ([]))[2 marks]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 4 @?= [] )
    , testCase "6 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 (Maybe))[2 marks]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 6 @?= Nothing )
    , testCase "8 in Branch (Leaf 3 300) (Leaf 7 700) 5 500 ([]))[1 mark]"
        (lkp (Branch (Leaf 3 300) (Leaf 7 700) 5 500) 8 @?= [] )
    ]


-- Tests for Part 3 (stats) -----------------------------------------------------

statsTests :: TF.Test
statsTests
 = testGroup "\nPart 3 - Tail Recursiive Stats (16 marks)\n"
    [ testCase "[] [1 mark]"
        ( getLASs 0 0 0 [] @?= (0,0,0) )
    , testCase "[0]  [1 mark]"
        ( getLASs 0 0 0 [0] @?= (1,0,0) )
    , testCase "[1] [1 mark]"
        ( getLASs 0 0 0 [1] @?= (1,1,1) )
    , testCase "[1,1] [1 mark]"
        ( getLASs 0 0 0 [1,1] @?= (2,2,2) )
    , testCase "[1,1,2] 0 [1 mark]"
        ( getLASs 0 0 0 [1,1,2] @?= (3,4,6) )
    , testCase "[0..10] 0 [1 mark]"
        ( getLASs 0 0 0 [0..10] @?= (11,55,385) )
    , testCase "[-2,2,-2,2,-2,2,-2,2] 0 [1 mark]"
       (getLASs 0 0 0 [-2,2,-2,2,-2,2,-2,2] @?= (8,0,32) )
    , testCase "[] (OFFSET) 0 [1 mark]"
       (getLASs 1000 1000 1000 [] @?= (1000,1000,1000) )
    , testCase "[0] (OFFSET) 0 [1 mark]"
       (getLASs 1000 1000 1000 [0] @?= (1001,1000,1000))
    , testCase "[1] (OFFSET)[1 mark]"
        (getLASs 1000 1000 1000 [1] @?= (1001,1001,1001))
    , testCase "[1,1,2] (OFFSET)[1 mark]"
       (getLASs 1000 1000 1000 [1,1,2] @?= (1003,1004,1006))
    , testCase "[0..10] (OFFSET)[1 mark]"
       (getLASs 1000 1000 1000 [0..10] @?= (1011,1055,1385))
    , testCase "[-2,2,-2,2,-2,2,-2,] (OFFSET)[1 mark]"
        (getLASs 1000 1000 1000 [-2,2,-2,2,-2,2,-2,2]
               @?= (1008,1000,1032))
    , testCase "getLASs is called correctly [3 marks]"
      (getLengthAndSums [0..10] @?= (11,55,385))
    ]
