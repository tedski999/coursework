{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

import Ex02
import Data.Maybe (fromJust)

main = defaultMain tests -- runs the tests

tests :: [TF.Test]
tests = [ testGroup "\n\nExercise 01 Tests (40 BB points)\n"
            [ teacherGotItRight
            , testGroup "\nDeclaration" [ testCase declaration (1+1 @?= 2) ]
            , evalTests
            , lawTests
            ]
        ]

-- sanity checking for supplied code

x42 = ("x",42.0)
y99 = ("y",99.0)
emptyD   =  []         ::  EDict
dx42     =  [x42]      ::  EDict
dy99     =  [y99]      ::  EDict
dx42y99  =  [y99,x42]  ::  EDict

teacherGotItRight :: TF.Test
teacherGotItRight
 = testGroup "\nSupplied Code Tests (using x=42, y=99) SHOULD NEVER FAIL! "
    [ testCase "Neither in empty dict"
        ( x42 `elem` emptyD  || y99 `elem` emptyD @?= False )
    , testCase "defining x=42 works" ( define emptyD "x" 42 @?= dx42 )
    , testCase "defining y=99 works" ( define emptyD "y" 99 @?= dy99)
    , testCase "defining x=42 and y=99 works"
      ( define (define emptyD "x" 42) "y" 99 @?= dx42y99)
    , testCase "find in empty dict fails" ( find emptyD "x" @?= Left "undefined var x" )
    , testCase "find x in dict with only y fails" ( find dy99 "x" @?= Left "undefined var x" )
    , testCase "find x in dict with only x succeeds"
        ( find dx42 "x" @?= Right 42.0 )
    , testCase "find x in dict with both succeeds"
        ( find dx42y99 "x" @?= Right 42.0 )
    , testCase "find y in dict with both succeeds"
        ( find dx42y99 "y" @?= Right 99.0 )
    , testCase "find z in dict with x,y fails"
        ( find dx42y99 "z" @?= Left "undefined var z" )
    , testCase "find z in dict after inserting it succeeds"
        ( find (define dx42y99 "z" 3.14159) "z" @?= Right 3.14159 )
    , testCase "find y in xy-dict after inserting z succeeds"
        ( find (define dx42y99 "z" 3.14159) "y" @?= Right 99.0 )
    ]

-- tests for Part 1 (eval) -----------------------------------------------------


d0 = Val 0.0
d1 = Val 1.0
d2 = Val 2.0
d42 = Val 42.0
d10 = Val 10.0
d99 = Val 99.0
d5 = Val 5.0
vx = Var "x"
vy = Var "y"
vz = Var "z"
dx42y7 = [("x",42.0),("y",7.0)]
n = "n"
va = Var "a"
vb = Var "b"
vc = Var "c"
vd = Var "d"


evalTests :: TF.Test
evalTests
 = testGroup "\nPart 1 - eval (50 marks, worth 25 BB points)\n"
    [ evalVTests   --  5
    , evalAddTests -- 10
    , evalSubTests -- 10
    , evalMulTests -- 10
    , evalDvdTests -- 10
    , evalDefTests --  5
    ]


evalVTests
 = testGroup "\nVal/Var tests (5 marks)\n"
    [ testCase "Val with empty Dict [1 mark]"
        (eval emptyD (Val 42.0) @?= Right 42.0)
    , testCase "Val with xy Dict [1 mark]"
        (eval dx42y99 (Val 42.0) @?= Right 42.0)
    , testCase "Var x with empty Dict [1 mark]"
        (eval emptyD (Var "x") @?= (Left "undefined var x"))
    , testCase "Var x with y Dict [1 mark]"
        (eval dy99 (Var "x") @?= (Left "undefined var x"))
    , testCase "Var y with y Dict [1 mark]"
        (eval dy99 (Var "y") @?= Right 99.0)
    ]


eBad = Dvd d1 d0

evalAddTests
 = testGroup "\nAdd tests (10 marks)\n"
    [ testCase "Add fail fail [1 mark]"
        (eval dx42y99 (Add eBad eBad) @?= (Left "div by zero"))
    , testCase "Add fail Val [1 mark]"
        (eval dx42y99 (Add eBad d2) @?= (Left "div by zero"))
    , testCase "Add Val fail  [1 mark]"
        (eval dx42y99 (Add d1 eBad) @?= (Left "div by zero"))
    , testCase "Add Val Var (empty dict) [1 mark]"
        (eval emptyD (Add d2 vx) @?= (Left "undefined var x"))
    , testCase "Add Val Var (dict with x) [1 mark]"
        (eval dx42y99 (Add d2 vx) @?= Right 44.0)
    , testCase "Add Var Val (empty dict) [1 mark]"
        (eval emptyD (Add vx d2) @?= (Left "undefined var x"))
    , testCase "Add Var Val (dict with x) [1 mark]"
        (eval dx42y99 (Add vx d2) @?= Right 44.0)
    , testCase "Add Var Var (dict with x) [1 mark]"
        (eval dx42 (Add vx vy) @?= (Left "undefined var y"))
    , testCase "Add Var Var (dict with y) [1 mark]"
        (eval dy99 (Add vx vy) @?= (Left "undefined var x"))
    , testCase "Add Var Var (dict with xy) [1 mark]"
        (eval dx42y99 (Add vx vy) @?= Right 141.0)
    ]

evalSubTests
 = testGroup "\nSub tests  (10 marks)\n"
    [ testCase "Sub fail fail [1 mark]"
        (eval dx42y99 (Sub eBad eBad) @?= (Left "div by zero"))
    , testCase "Sub fail Val [1 mark]"
        (eval dx42y99 (Sub eBad d2) @?= (Left "div by zero"))
    , testCase "Sub Val fail  [1 mark]"
        (eval dx42y99 (Sub d1 eBad) @?= (Left "div by zero"))
    , testCase "Sub Val Val (empty dict) [1 mark]"
        (eval emptyD (Sub d2 d1) @?= Right 1.0)
    , testCase "Sub Val Val (non-empty dict) [1 mark]"
        (eval dx42y99 (Sub d1 d2) @?= Right (-1.0))
    , testCase "Sub Val Var (empty dict) [1 mark]"
        (eval emptyD (Sub d2 vx) @?= (Left "undefined var x"))
    , testCase "Sub Val Var (dict with x) [1 mark]"
        (eval dx42y99 (Sub d2 vx) @?= Right (-40.0))
    , testCase "Sub Var Val (empty dict) [1 mark]"
        (eval emptyD (Sub vx d2) @?= (Left "undefined var x"))
    -- , testCase "Sub Var Val (dict with x) [1 mark]"
    --     (eval dx42y99 (Sub vx d2) @?= Right 40.0)
    , testCase "Sub Var Var (dict with x) [1 mark]"
        (eval dx42 (Sub vx vy) @?= (Left "undefined var y"))
    -- , testCase "Sub Var Var (dict with y) [1 mark]"
    --     (eval dy99 (Sub vx vy) @?= (Left "undefined var x"))
    , testCase "Sub Var Var (dict with xy) [1 mark]"
        (eval dx42y99 (Sub vx vy) @?= Right (-57.0))
    ]

evalMulTests
 = testGroup "\nMul tests (10 marks)\n"
    [ testCase "Mul fail fail [1 mark]"
        (eval dx42y99 (Mul eBad eBad) @?= (Left "div by zero"))
    , testCase "Mul fail Val [1 mark]"
        (eval dx42y99 (Mul eBad d2) @?= (Left "div by zero"))
    , testCase "Mul Val fail  [1 mark]"
        (eval dx42y99 (Mul d1 eBad) @?= (Left "div by zero"))
    , testCase "Mul Val Val (empty dict) [1 mark]"
        (eval emptyD (Mul d2 d1) @?= Right 2.0)
    , testCase "Mul Val Val (non-empty dict) [1 mark]"
        (eval dx42y99 (Mul d1 d2) @?= Right 2.0)
    -- , testCase "Mul Val Var (empty dict) [1 mark]"
    --     (eval emptyD (Mul d2 vx) @?= (Left "undefined var x"))
    , testCase "Mul Val Var (dict with x) [1 mark]"
        (eval dx42y99 (Mul d2 vx) @?= Right 84.0)
    , testCase "Mul Var Val (empty dict) [1 mark]"
        (eval emptyD (Mul vx d2) @?= (Left "undefined var x"))
    -- , testCase "Mul Var Val (dict with x) [1 mark]"
    --     (eval dx42y99 (Mul vx d2) @?= Right 84.0)
    , testCase "Mul Var Var (dict with x) [1 mark]"
        (eval dx42 (Mul vx vy) @?= (Left "undefined var y"))
    , testCase "Mul Var Var (dict with y) [1 mark]"
        (eval dy99 (Mul vx vy) @?= (Left "undefined var x"))
    , testCase "Mul Var Var (dict with xy) [1 mark]"
        (eval dx42y99 (Mul vx vy) @?= Right 4158.0)
    ]

evalDvdTests
 = testGroup "\nDvd tests  (10 marks)\n"
    [ testCase "Dvd fail fail [1 mark]"
        (eval dx42y99 (Dvd eBad eBad) @?= (Left "div by zero"))
    , testCase "Dvd fail Val [1 mark]"
        (eval dx42y99 (Dvd eBad d2) @?= (Left "div by zero"))
    -- , testCase "Dvd Val fail  [1 mark]"
    --     (eval dx42y99 (Add d1 eBad) @?= (Left "div by zero"))
    , testCase "Dvd Val Zero (empty dict) [1 mark]"
        (eval emptyD (Dvd d2 d0) @?= (Left "div by zero"))
    , testCase "Dvd Val Val (empty dict) [1 mark]"
        (eval emptyD (Dvd d2 d1) @?= Right 2.0)
    , testCase "Dvd Val Val (non-empty dict) [1 mark]"
        (eval dx42y99 (Dvd d1 d2) @?= Right 0.5)
    -- , testCase "Dvd Val Var (empty dict) [1 mark]"
    --     (eval emptyD (Dvd d2 vx) @?= (Left "undefined var x"))
    , testCase "Dvd Val Var (dict with x) [1 mark]"
        (eval [("z",3.0)] (Dvd d42 vz) @?= Right 14.0)
    , testCase "Dvd Var Val (empty dict) [1 mark]"
        (eval emptyD (Dvd vx d2) @?= (Left "undefined var x"))
    -- , testCase "Dvd Var Val (dict with x) [1 mark]"
    --     (eval dx42y99 (Dvd vx d2) @?= Right 21.0)
    , testCase "Dvd Var Var (dict with x) [1 mark]"
        (eval dx42 (Dvd vx vy) @?= (Left "undefined var y"))
    , testCase "Dvd Var Var (dict with y) [1 mark]"
        (eval dy99 (Dvd vx vy) @?= (Left "undefined var x"))
    , testCase "Dvd Var Var (dict with xy) [1 mark]"
        (eval dx42y7 (Dvd vx vy) @?= Right 6.0)
    ]


evalDefTests
 = testGroup "\nDef tests (5 marks)\n"
    [ testCase "Def n fail Val [1 mark]"
        (eval dx42y99 (Def n eBad d2) @?= (Left "div by zero"))
    , testCase "Def n Val fail  [1 mark]"
        (eval dx42y99 (Add d1 eBad) @?= (Left "div by zero"))
    -- , testCase "Def n Val Val (empty dict) [1 mark]"
    --     (eval emptyD (Def n d1 d2) @?= Right 2.0)
    , testCase "Def n Val x (empty dict) [1 mark]"
        (eval emptyD (Def n d1 vx) @?= (Left "undefined var x"))
    , testCase "Def n Val x (dict with x) [1 mark]"
        (eval dx42 (Def n d1 vx) @?= Right 42.0)
    -- , testCase "Def n Val n (empty dict) [1 mark]"
    --     (eval emptyD (Def n d1 (Var n)) @?= Right 1.0)
    , testCase "a=2 b=42 in a*b [1 mark]"
        (eval emptyD (Def "a" d2 (Def "b" d42 (Mul va vb))) @?= Right 84.0)
    ]

-- Tests for Part 2 (other) -----------------------------------------------------

lawTests :: TF.Test
lawTests
 = testGroup "\nPart 2 - laws (15 marks, worth 15 BB points)\n"
    [ law1Tests
    , law2Tests
    , law3Tests
    , law4Tests
    ]

law1Tests
 = testGroup "\nLaw 1 tests (3 marks)\n"
    [ testCase "1+2 is 2+1 [1 mark]"
        (law1 (Add (Val 1) (Val 2)) @?= Just (Add (Val 2) (Val 1)))
    , testCase "Cannot handle 1*2 [1 mark]"
        (law1  (Mul (Val 1) (Val 2)) @?= Nothing)
    , testCase "x+y=y+x [1 mark]"
        (eval dx42y99 (fromJust $ law1  (Add vx vy))
          @?= eval dx42y99 (Add vy vx))
    ]

law2Tests
 = testGroup "\nLaw 2 tests (3 marks)\n"
    [ testCase "(1+2)+3 is 1+(2+3) [1 mark]"
        ( law2 (Add (Add (Val 1) (Val 2)) (Val 3))
          @?= Just (Add (Val 1) (Add (Val 2) (Val 3))) )
    , testCase "Cannot handle 1/(2/3) [1 mark]"
        ( law2  (Dvd (Val 1) (Dvd (Val 2) (Val 3))) @?= Nothing)
    , testCase "(x+y)+7=x+(y+7) [1 mark]"
        (eval dx42y99 (fromJust $ law2  (Add (Add vx vy) (Val 7)) )
          @?= eval dx42y99 (Add vx (Add vy (Val 7))) )
    ]


law3Tests
 = testGroup "\nLaw 3 tests (3 marks)\n"
    [ testCase "(1-2)-3 is 1-(2+3) [1 marks]"
        ( law3 (Sub (Sub (Val 1) (Val 2)) (Val 3))
          @?= Just (Sub (Val 1) (Add (Val 2) (Val 3))) )
    , testCase "Cannot handle (2/3)/1 [1 mark]"
        ( law3  (Dvd (Dvd (Val 2) (Val 3)) (Val 1) ) @?= Nothing)
    , testCase "(x-y)-7 = x-(y+7) [1 mark]"
        (eval dx42y99 (fromJust $ law3 (Sub (Sub vx vy) (Val 7)) )
          @?= eval dx42y99 (Sub vx (Add vy (Val 7))) )
    ]


law4Tests
 = testGroup "\nLaw 4 tests (6 marks)\n"
    [ testCase "99*99-42*42 = (99+42)*(99-42) [2 marks]"
        ( law4 (Sub (Mul (Val 99) (Val 99)) (Mul (Val 42) (Val 42)))
          @?= Just (Mul (Add (Val 99) (Val 42)) (Sub (Val 99) (Val 42))) )
    , testCase "Cannot handle (99*42)-(99*42) [2 marks]"
        ( law4  (Sub (Mul (Val 99) (Val 42)) (Mul (Val 99) (Val 42))) @?= Nothing )
    , testCase "(y+x)*(y-x) is y*y-x*x [2 marks]"
        (eval dx42y99 (fromJust $ law4  (Sub (Mul vy vy) (Mul vx vx)) )
          @?= eval dx42y99  (Mul (Add vy vx) (Sub vy vx)) )
    ]
