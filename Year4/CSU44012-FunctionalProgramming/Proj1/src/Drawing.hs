module Drawing (
    Drawing (Circle, Ellipse, Square, Rectangle, Polygon, Translate, Rotate, Scale, Shear, Colour, Mask),
    blank, black, white, red, green, blue,
    render
) where

import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.ByteString.Lazy (ByteString)
import Codec.Picture (PixelRGBA8(PixelRGBA8), encodePng, readPixel, writePixel)
import Codec.Picture.Types (MutableImage, createMutableImage, mutableImageWidth, mutableImageHeight, freezeImage)

type Vec2 = (Double, Double)
type Vec3 = (Double, Double, Double)
type Vec4 = (Double, Double, Double, Double)
type Mat3 = (Vec3, Vec3, Vec3)


-----------------------
-- Drawing Structure --
-----------------------

-- A drawing is a tree of transformations and colours with shapes as leaves.
data Drawing = Circle
             | Ellipse   Vec2
             | Square
             | Rectangle Vec2
             | Polygon   [Vec2]
             | Translate Vec2   [Drawing]
             | Rotate    Double [Drawing]
             | Scale     Vec2   [Drawing]
             | Shear     Vec2   [Drawing]
             | Colour    Vec4   [Drawing]
             | Mask      [Drawing] [Drawing]
             deriving Show

draw :: PrimMonad m => Drawing -> Mat3 -> PixelRGBA8 -> MutableImage (PrimState m) PixelRGBA8 -> m ()
-- Shapes are defined as mathematical functions
draw          (Circle) t c i = drawShape (ellipse (1,1)) t c i
draw       (Ellipse s) t c i = drawShape (ellipse s) t c i
draw          (Square) t c i = drawShape (polygon [(1,1),(-1,1),(-1,-1),(1,-1)]) t c i
draw (Rectangle (w,h)) t c i = drawShape (polygon [(w,h),(-w,h),(-w,-h),(w,-h)]) t c i
draw      (Polygon ps) t c i = drawShape (polygon ps) t c i
-- Child transformations are multiplied together
draw (Translate nt ds) t c i = mapM_ (\d -> draw d tt c i) ds where tt = multMat3 t (translate nt)
draw    (Rotate nr ds) t c i = mapM_ (\d -> draw d tt c i) ds where tt = multMat3 t (rotate nr)
draw     (Scale ns ds) t c i = mapM_ (\d -> draw d tt c i) ds where tt = multMat3 t (scale ns)
draw     (Shear ns ds) t c i = mapM_ (\d -> draw d tt c i) ds where tt = multMat3 t (shear ns)
-- Colours are blended based on the alpha channel of the new colour
draw    (Colour nc ds) t c i = mapM_ (\d -> draw d t cc i) ds where cc = blendPixel c (rgba nc)
draw    (Mask ds1 ds2) t c i = maskDrawing ds1 ds2 t c i


---------------
-- Rendering --
---------------

-- Render trees of drawings to a PNG encoded lazy ByteString
render :: [Drawing] -> Vec2 -> ByteString
render ds (w,h) = encodePng $ runST $ do
    i <- createMutableImage (floor w) (floor h) (rgba blank)
    mapM_ (\d -> draw d ((1,0,0),(0,1,0),(0,0,1)) (rgba black) i) ds
    freezeImage i

-- Draw a shape to the image with a specified transformation and colour (optimised to check if only relevant pixels are within the shape)
drawShape :: PrimMonad m => (Vec2 -> Bool) -> Mat3 -> PixelRGBA8 -> MutableImage (PrimState m) PixelRGBA8 -> m ()
drawShape f t c i = mapM_ drawPixel (getRelevantPixels t i) where
    drawPixel ((x,y),uv) = if f uv then do p <- readPixel i x y; writePixel i x y (blendPixel p c) else return ()

-- Return a list of all pixels and corresponding shape transformation points present in the image
maskDrawing :: PrimMonad m => [Drawing] -> [Drawing] -> Mat3 -> PixelRGBA8 -> MutableImage (PrimState m) PixelRGBA8 -> m ()
maskDrawing ds1 ds2 t c i = do
    -- Draw both the drawing and the mask
    d <- createMutableImage w h (rgba blank); mapM_ (\d1 -> draw d1 t c d) ds1
    m <- createMutableImage w h (rgba blank); mapM_ (\d2 -> draw d2 t c m) ds2
    -- Copy the drawing to the image while applying the mask alpha channel
    mapM_ (drawMaskedPixel d m) (getAllPixels t i) where
        (w,h) = (mutableImageWidth i, mutableImageHeight i)
        drawMaskedPixel d m ((x,y),_) = do
            p <- readPixel i x y
            dp <- readPixel d x y
            mp <- readPixel m x y
            writePixel i x y (blendPixel p (maskPixel dp mp))

-- Return a list of all pixels and corresponding shape transformation points present in the image
getAllPixels :: Mat3 -> MutableImage s a -> [((Int,Int), Vec2)]
getAllPixels t i = [ ((x,y), invertTransform t i (x,y)) | x <- [0..w-1], y <- [0..h-1] ] where
    (w,h) = (mutableImageWidth i, mutableImageHeight i)

-- Return a list of pixels and corresponding shape transformation points which might contain the shape
getRelevantPixels :: Mat3 -> MutableImage s a -> [((Int,Int), Vec2)]
getRelevantPixels t i = [ ((x,y), invertTransform t i (x,y)) | x <- [minX..maxX], y <- [minY..maxY] ] where
    (w,h) = (fromIntegral $ mutableImageWidth i, fromIntegral $ mutableImageHeight i)
    -- Clip points outside a 2x2 region around the shapes origin and pixels outside the image
    clamp a = min 1 $ max (-1) a
    (x0,y0) = (clamp x, clamp y) where (x,y,_) = multMatVec3 t (-1,-1, 1)
    (x1,y1) = (clamp x, clamp y) where (x,y,_) = multMatVec3 t ( 1,-1, 1)
    (x2,y2) = (clamp x, clamp y) where (x,y,_) = multMatVec3 t (-1, 1, 1)
    (x3,y3) = (clamp x, clamp y) where (x,y,_) = multMatVec3 t ( 1, 1, 1)
    -- Determine the top left and bottom right pixel coordinates to generate the Cartesian Product between
    (minX, maxX) = (floor $ (w-1) * (1 + minimum xs) / 2, floor $ (w-1) * (1 + maximum xs) / 2) where xs = [x0,x1,x2,x3]
    (minY, maxY) = (floor $ (h-1) * (1 + minimum ys) / 2, floor $ (h-1) * (1 + maximum ys) / 2) where ys = [y0,y1,y2,y3]


------------------------
-- Fundamental Shapes --
------------------------

-- Test if a point xy is inside a unit ellipse with semi-major length a and semi-minor length b
ellipse :: Vec2 -> Vec2 -> Bool
ellipse (a,b) (x,y) = x ** 2 / a ** 2 + y ** 2 / b ** 2 <= 1

-- Test if a point xy is inside a polygon consisting of n points using the polygon ray-casting algorithm
polygon :: [Vec2] -> Vec2 -> Bool
polygon points xy = p (points ++ [head points]) xy False where
    p ((x0,y0):(x1,y1):ps) (x,y) ok | (y1 > y) /= (y0 > y) && x < (x0-x1) * (y-y1) / (y0-y1) + x1 = p ((x1,y1):ps) (x,y) (not ok)
                                    |                                                   otherwise = p ((x1,y1):ps) (x,y) ok
    p _ _ ok = ok


-----------------
-- Matrix Math --
-----------------

-- Multiply 3x3 matrix and 3x1 vector
multMatVec3 :: Mat3 -> Vec3 -> Vec3
multMatVec3 ((a,b,c),(d,e,f),(g,h,i)) (x,y,z) = (a*x + b*y + c*z, d*x + e*y + f*z, g*x + h*y + i*z)

-- Multiply two 3x3 matrices
multMat3 :: Mat3 -> Mat3 -> Mat3
multMat3 ((a,b,c),(d,e,f),(g,h,i)) ((p,q,r),(s,t,w),(x,y,z)) = (
    (a*p + b*s + c*x, a*q + b*t + c*y, a*r + b*w + c*z),
    (d*p + e*s + f*x, d*q + e*t + f*y, d*r + e*w + f*z),
    (g*p + h*s + i*x, g*q + h*t + i*y, g*r + h*w + i*z))

-- Invert 3x3 affine quickly by inverting top-left 2x2 minor and also multiplying translation vector by result
invertAffine :: Mat3 -> Mat3
invertAffine ((a,b,c),(d,e,f),(_,_,_)) = ((a',b',c'), (d',e',f'), (0,0,1)) where
    -- Invert top-left 2x2 minor
    det = 1/(a*e-b*d)
    ((a',b'),(d',e')) = ((e*det, -b*det), (-d*det, a*det))
    -- Multiply translation vector by inverted minor
    (c',f') = (a'*c+b'*f, d'*c+e'*f)

-- Invert affine transformation over image
invertTransform :: Mat3 -> MutableImage s a -> (Int,Int) -> (Double, Double)
invertTransform t i (x,y) = (a*u + b*v + c, d*u + e*v + f) where
    (w,h) = (fromIntegral $ mutableImageWidth i, fromIntegral $ mutableImageHeight i)
    ((a,b,c),(d,e,f),_) = invertAffine t
    (u,v) = ((1 - fromIntegral x/w*2), (1 - fromIntegral y/h*2))

-------------------------------
-- 2D Affine Transformations --
-------------------------------

translate, scale, shear :: Vec2 -> Mat3
translate (x,y) = ((1, 0, x), (0, 1, y), (0, 0, 1))
scale     (x,y) = ((x, 0, 0), (0, y, 0), (0, 0, 1))
shear     (x,y) = ((1, x, 0), (y, 1, 0), (0, 0, 1))
rotate :: Double -> Mat3
rotate x = ((cos x, -sin x, 0), (sin x, cos x, 0), (0, 0, 1))


-------------
-- Colours --
-------------

-- Some useful colours
blank, white, black, red, green, blue :: Vec4
blank = (0,   0,   0,   0  )
white = (255, 255, 255, 255)
black = (0,   0,   0  , 255)
red   = (255, 0,   0  , 255)
green = (0,   255, 0  , 255)
blue  = (0,   0,   255, 255)

-- Convert Double 4-tuple to PixelRGBA8
rgba :: Vec4 -> PixelRGBA8
rgba (r,g,b,a) = (PixelRGBA8 (floor r) (floor g) (floor b) (floor a))

-- Merge the colours of two pixels based on the alpha of the new pixel
blendPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
blendPixel (PixelRGBA8 r0 g0 b0 a0) (PixelRGBA8 r1 g1 b1 a1) = (PixelRGBA8 (f r0 r1) (f g0 g1) (f b0 b1) (f a0 a1)) where
    a = fromIntegral a1 / 255 :: Double
    f c0 c1 = floor (fromIntegral c0 * (1-a) + fromIntegral c1 * a)

-- Influence the transparency of a pixel colour based on the alpha of the masking pixel
maskPixel :: PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8
maskPixel (PixelRGBA8 r g b da) (PixelRGBA8 _ _ _ ma) = (PixelRGBA8 r g b a) where
    a = (floor (fromIntegral da * fromIntegral ma / 255 :: Double))
