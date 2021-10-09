module Ex01 where
import Data.Char (toUpper)

name, idno, username :: String
name      =  "Ted Johnson"  -- replace with your name
idno      =  "19335618"    -- replace with your student id
username  =  "edjohnso"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]


{- Part 1

Write a function 'raise' that converts a string to uppercase

Hint: 'toUpper :: Char -> Char' converts a character to uppercase
if it is lowercase. All other characters are unchanged.
It is imported should you want to use it.

-}
raise :: String -> String
raise str = map toUpper str

{- Part 2

Write a function 'nth' that returns the nth element of a list.
Hint: the test will answer your Qs

-}
nth :: Int -> [a] -> a
nth i xs = xs !! (i - 1)

{- Part 3

Write a function `commonLen` that compares two sequences
and reports the length of the prefix they have in common.

-}
commonLen :: Eq a => [a] -> [a] -> Int
commonLen _ [] = 0
commonLen [] _ = 0
commonLen (x:xs) (y:ys) | x == y = 1 + commonLen xs ys
                        | x /= y = 0

{- Part 4

(TRICKY!) (VERY!)

Write a function `runs` that converts a list of things
into a list of sublists, each containing elements of the same value,
which when concatenated together give the same list

So `runs [1,2,2,1,3,3,3,2,2,1,1,4]`
 becomes `[[1],[2,2],[1],[3,3,3],[2,2],[1,1],[4]]`

Hint:  `elem :: Eq a => a -> [a] -> Bool`

HINT: Don't worry about code efficiency
       Seriously, don't!

-}
runs :: Eq a => [a] -> [[a]]
runs [] = []
runs xs@(x:_) = let (run, rest) = span (x ==) xs  -- splits xs into the lists run and rest where run is a list of repeating elements prefixing xs and rest is the remaining elements in xs
                 in run : runs rest               -- concat this run with all the runs that come after in the remaining elements

