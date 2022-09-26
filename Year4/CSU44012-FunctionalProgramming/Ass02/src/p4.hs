module Main (main) where

-- Keep evaluating a until resolves false
while :: IO Bool -> IO ()
while a = do
    result <- a
    if result then while a else return ()

-- Example function
nonEmptyLine :: IO Bool
nonEmptyLine = do
    line <- getLine
    return $ length line /= 0

main :: IO ()
main = while nonEmptyLine
