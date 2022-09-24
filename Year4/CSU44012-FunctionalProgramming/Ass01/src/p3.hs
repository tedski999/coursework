module Main (main) where

import Control.Parallel

parallel_map :: (a -> b) -> [a] -> [b]
parallel_map _ [] = []
parallel_map f (x:xs) = fx `par` (pfx `pseq` fx : pfx)
                        where fx = f x; pfx = parallel_map f xs

fact :: Integer -> Integer
fact 1 = 1
fact x = fact (x-1) * x

main :: IO ()
main = print $ parallel_map fact [1..2000]
