module Render (Window, window, render) where

import Codec.Picture
import Shapes

--  A window specifies what part of the world to render and at which resolution.
--  Values are top left & bottom right corner to be rendered, and the size of the output device to render into
data Window = Window Point Point (Int,Int)

window :: Point -> Point -> (Int,Int) -> Window
window = Window

-- Render a drawing into an image, then save into a file
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window tl br (w,h) = win

      -- Solution for part 3: Map screen pixels to world points by lerping between top-left and bottom-right corners
      pixRenderer :: Int -> Int -> PixelRGB8
      pixRenderer x y = PixelRGB8 c c c
          where
            c = colorForImage $ point wx wy
            wx = getX br * dx + getX tl * (1-dx)
            wy = getY br * dy + getY tl * (1-dy)
            dx = fromIntegral x / fromIntegral w
            dy = fromIntegral y / fromIntegral h

      colorForImage :: Point -> Pixel8
      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0
