{-# LANGUAGE InstanceSigs #-}

module Main (main) where

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Applicative List where
    pure :: a -> List a
    pure = return
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) fs as = fs >>= (\f -> as >>= (\a -> return $ f a))

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) Nil _ = Nil
    (>>=) (Cons h t) f = catLists (f h) (t >>= f)
    return :: a -> List a
    return x = Cons x Nil

catLists :: List a -> List a -> List a
catLists Nil ys = ys
catLists (Cons x xs) ys = Cons x (catLists xs ys)

printList :: Show a => List a -> IO ()
printList Nil = return ()
printList (Cons h t) = print h >> printList t

myList :: List Integer
myList = Cons 5 $ Cons 20 $ Cons 1 $ Nil

main :: IO ()
main = printList $ myList >>= (\x -> return $ x * 2)
