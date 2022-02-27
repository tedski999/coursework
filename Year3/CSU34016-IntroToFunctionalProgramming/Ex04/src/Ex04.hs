{- butrfeld Andrew Butterfield -}
module Ex04 where

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

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
ins k d Empty = Leaf k d
ins k d (Leaf lk ld) | k < lk  = Branch (Leaf k d) Empty lk ld
                     | k > lk  = Branch Empty (Leaf k d) lk ld
                     | k == lk = Leaf k d
ins k d (Branch l r bk bd) | k < bk  = Branch (ins k d l) r bk bd
                           | k > bk  = Branch l (ins k d r) bk bd
                           | k == bk = Branch l r k d

-- Part 2 : Tree Lookup -------------------------------

lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp Empty _ = fail "No such key"
lkp (Leaf lk ld) k | k == lk   = return ld
                   | otherwise = lkp Empty k
lkp (Branch l r bk bd) k | k < bk  = lkp l k
                         | k > bk  = lkp r k
                         | k == bk = return bd

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds = getLASs init1 init2 init3 ds
init1 = 0
init2 = 0
init3 = 0

{-
  Implement the following tail-recursive helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs len listsum sumofsquares []     = (len, listsum, sumofsquares)
getLASs len listsum sumofsquares (d:ds) = getLASs (len + 1) (listsum + d) (sumofsquares + d**2) ds

-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)
