module Shapes (
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, mandelbrotSet,
  identity, translate, rotate, scale, (<+>),
  inside) where

-- Utilities

data Vector = Vector Double Double
              deriving Show

vector :: Double -> Double -> Vector
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX :: Vector -> Double
getX (Vector x _) = x

getY :: Vector -> Double
getY (Vector _ y) = y

-- Shapes

type Point = Vector

point :: Double -> Double -> Point
point = vector

data Shape = Empty
           | Circle
           | Square
           | MandelbrotSet Int
             deriving Show

empty, circle, square :: Shape
empty = Empty
circle = Circle
square = Square

mandelbrotSet :: Int -> Shape
mandelbrotSet n = MandelbrotSet n

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity :: Transform
identity = Identity

translate :: Vector -> Transform
translate = Translate

scale :: Vector -> Transform
scale = Scale

rotate :: Double -> Transform
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

(<+>) :: Transform -> Transform -> Transform
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape)]

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
_ `insides` Empty  = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` MandelbrotSet n = approxTest n p

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

-- Solution for part 4: Adapt Mark P. Jones Mandelbrot set implementation

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

fairlyClose :: Vector -> Bool
fairlyClose (Vector u v) = (u * u + v * v) < 100

mandelbrot :: Vector -> [Vector]
mandelbrot p = iterate (next p) (Vector 0 0)

next :: Vector -> Vector -> Vector
next (Vector u v) (Vector x y) = Vector (x * x - y * y + u) (2 * x * y + v)
