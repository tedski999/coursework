module Main (main) where

import Shapes
import Render (Window, window, render)

-- Where to center image on complex plane
position :: Point
position = point (-0.325) 0.45
-- How much to scale image up
zoom :: Double
zoom = 25.0
-- How many iterations to perform
percision :: Int
percision = 100
-- How many pixels wide to generate image
quality :: Int
quality = 1500

w :: Window
w = window (point (-1.5) (-1.5)) (point 1.5 1.5) (quality,quality)

d :: [(Transform, Shape)]
d = [ (scale (point zoom zoom) <+> translate position, mandelbrotSet percision) ]

main :: IO ()
main = render "output.png" w d
