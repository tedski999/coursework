module Main (main) where

f1 :: [IO a] -> IO a
-- I can't figure out how to "return an anything action"...
-- I thought something like this would serve as an identify action:
-- f1 [] = return ()
-- A partial solution is to just skip the empty list case:
f1 (a:[]) = a
f1 (a:as) = a >> f1 as

main :: IO ()
main = f1 actions
        where actions = [ putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o' ]
