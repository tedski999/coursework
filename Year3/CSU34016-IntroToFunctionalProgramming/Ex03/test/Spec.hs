module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)

import Control.Monad

import AST
import HParse

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\nreasonH tests"
            [ parseTests
            ]
        ]

assertNotEqual :: (Eq a, Show a) => String -> a -> a -> Assertion
assertNotEqual preface expected actual =
  unless (actual /= expected) (assertFailure msg)
 where msg = (if null preface then "" else preface ++ "\n") ++
             "didn't want: " ++ show expected ++ "\n but got it."

(@?/=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?/= expected = assertNotEqual "" expected actual


-- parseExpr :: Monad m => ParseMode -> Lines -> Parser m Expr
eparse :: String -> Maybe (Expr,Lines)
eparse estr = parseExpr (ParseMode "<test>") [] [(1,estr)]

parseTests :: TF.Test
parseTests
 = testGroup "\nParsing Tests"
    [ testCase "'1:2:[]' parses  as '1:(2:[])' "
        ( eparse "1:2:[]" @?= eparse "1:(2:[])" )
    , testCase "'1:2:[]' not same as '(1:2):[]' "
       ( eparse "1:2:[]" @?/= eparse "(1:2):[]" )
    , testCase "'[1]++[2]++[3]' parses  as '([1]++[2])++[3]' "
       ( eparse "[1]++[2]++[3]" @?= eparse "([1]++[2])++[3]" )
    , testCase "'[1]++[2]++[3]' not same as '[1]++([2]++[3])' "
       ( eparse "[1]++[2]++[3]" @?/= eparse "[1]++([2]++[3])" )
    , testCase "'1+2+3' parses  as '(1+2)+3' "
       ( eparse "1+2+3" @?= eparse "(1+2)+3" )
    , testCase "'1+2+3' not same as '1+(2+3)' "
       ( eparse "1+2+3" @?/= eparse "1+(2+3)" )
    ]
