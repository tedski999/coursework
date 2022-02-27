module List where

length []      =  0                -- length.1
length (_:xs)  =  1 + length xs    -- length.2

[] ++ ys      =  ys                -- ++.1
(x:xs) ++ ys  =  x:(xs ++ ys)      -- ++.2

product []      =  1               -- product.1
product (x:xs)  =  x * product xs  -- product.2

sum []      =  0           -- sum.1
sum (x:xs)  =  x + sum xs  -- sum.2
