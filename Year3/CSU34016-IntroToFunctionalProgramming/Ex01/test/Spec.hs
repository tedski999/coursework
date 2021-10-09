{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

import Ex01

main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\n\nExercise 01 Tests (40 marks)\n"
            [ part1Tests
            , part2Tests
            , part3Tests
            , part4Tests
            ]
        ]


part1Tests :: TF.Test
part1Tests
 = testGroup "\nPart 1 - raise (10 marks)\n"
    [ testCase "raise up [2 marks]" (raise "up" @?= "UP")
    , testCase "raise null [2 marks]" (raise "" @?= "")
    , testCase "raise numbers [2 marks]" (raise "1234" @?= "1234")
    , testCase "raise single [2 marks]" (raise "a" @?= "A")
    , testCase "raise mixed [2 marks]" (raise "1aB2cD" @?= "1AB2CD")
    ]

part2Tests :: TF.Test
part2Tests
 = testGroup "\nPart 2 - nth (10 marks)\n"
    [ testCase "1st degree [2 marks]" (nth 1 "degree" @?= 'd')
    , testCase "2nd degree [2 marks]" (nth 2 "degree" @?= 'e')
    , testCase "4th degree [2 marks]" (nth 4 "degree" @?= 'r')
    , testCase "7th degrees [2 marks]" (nth 7 "degrees" @?= 's')
    , testCase "1st among equals" (nth 1 [42] @?= 42)
    ]

part3Tests :: TF.Test
part3Tests
 = testGroup "\nPart 3 - commonLen (10 marks)\n"
    [ testCase "common people [2 marks]"
        (commonLen "common people" "common people like me" @?= 13)
    , testCase "1st is null [1 mark]" (commonLen [] [1,2] @?= 0)
    , testCase "2nd is null [1 mark]" (commonLen [1,2] [] @?= 0)
    , testCase "1st is shorter [1 mark]" (commonLen [1,2] [1,2,3,4] @?= 2)
    , testCase "2nd is shorter [1 mark]" (commonLen [1,2,3] [1] @?= 1)
    , testCase "both are same [1 mark]" (commonLen [1,2,3] [1,2,3] @?= 3)
    , testCase "both differ - same length [1 mark]"
        (commonLen [1,2,3] [4,5,6] @?= 0)
    , testCase "complicated [1 mark]"
        (commonLen [[],"x","xy"] [[],"a"] @?= 1)
    , testCase "complicated [1 mark]"
        (commonLen [[1,2],[],[3],[4,5]] [[1,2],[],[3,4,5]] @?= 2)
    ]

part4Tests :: TF.Test
part4Tests
 = testGroup "\nPart 3 - runs (10 marks)\n"
    [ testCase "run rabbit run [3 marks]"
        ( runs [1,2,2,1,3,3,3,2,2,1,1,4]
          @?= [[1],[2,2],[1],[3,3,3],[2,2],[1,1],[4]] )
    , testCase "runs null [1 marks]" (runs [] @?= ([]::[[Int]]))
    , testCase "runs single [1 marks]" (runs [42] @?= [[42]])
    , testCase "runs singles [1 marks]" (runs [1,2,3] @?= [[1],[2],[3]])
    , testCase "runs double [1 marks]" (runs [42,42] @?= [[42,42]])
    , testCase "runs doubles [1 marks]" (runs [42,42,99,99] @?= [[42,42],[99,99]])
    , testCase "runs nulls [2 marks]" (runs [([]::[Int]),[],[]] @?= [[[],[],[]]])
    ]
