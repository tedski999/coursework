{-# LANGUAGE InstanceSigs #-}

module Main (main) where

data Pair a b = P a b

instance Functor (Pair a) where
    fmap :: (b -> c) -> Pair a b -> Pair a c
    fmap f (P a b) = P a (f b)

-- instance Applicative (Pair a) where
    -- Interesting signiture... We can't generate a value of type a out of thin air!
    -- pure :: b -> Pair a b
    -- pure x = P a x
    -- (<*>) :: Pair a (b -> c) -> Pair a b -> Pair a c
    -- (<*>) (P a f) (P _ b) = P a (f b)

printPair :: Show a => Show b => Pair a b -> IO ()
printPair (P a b) = print a >> print b

myPair :: Pair Integer Integer
myPair = P 10 20

main :: IO ()
main = printPair $ fmap (* 2) myPair
