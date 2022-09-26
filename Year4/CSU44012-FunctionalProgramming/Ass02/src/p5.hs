module Main (main) where

-- Evaluate every action in list by evaluating the head and concatenating the evaluation of the tail
f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 (a:as) = do
    x <- a
    xs <- f2 as
    return (x:xs)

-- Example application
read10 :: IO String
read10 = f2 $ take 3 actions
          where actions = getChar : actions

main :: IO ()
main = do
    s <- read10
    putStrLn s
