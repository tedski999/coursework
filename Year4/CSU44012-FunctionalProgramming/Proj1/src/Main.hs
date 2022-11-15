{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Drawing

import Data.Text.Lazy (Text, append, pack)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Base64 (encodeBase64)

import qualified Web.Scotty as S
import Text.Blaze.Html5 (body, h2, a, br, img, textarea, toHtml, toValue, (!))
import Text.Blaze.Html5.Attributes (href, src, rows, cols)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- Image A demonstrates shapes, colours and transformations
imageA :: [Drawing]
imageA = [
        Scale (0.75,0.75) [ Colour green [
            Circle,
            Scale (0.5,0.5) [
                Translate (1.5,0) [ Colour (255,100,255,100) [ Rotate 0.2 [ Square ] ] ],
                Translate (-1.5,1) [ Colour (0,0,0,200) [ Ellipse (0.5,1.0) ] ]
            ]
        ] ]
    ]

-- Image B uses masks to create complex shapes
imageB :: [Drawing]
imageB = [
        Scale (0.85,0.85) [ Colour (100,100,0,255) [ Square ] ],
        Mask [Colour (0,50,100,255) [Square]] [Colour (0,0,0,200) [Circle]],
        Rotate (pi/2) [
            Mask
                [ Scale (0.3,0.5) [ Colour red [ Square ] ] ]
                [ Scale (0.2,0.2) [
                    Colour (0,0,0,200) [ Translate (1.5,1) [ Circle ] ],
                    Translate (0.5,-0.7) [ Scale (2,2) [ Rotate (pi/8) [ Rectangle (0.5,1.0) ] ] ],
                    Colour (0,0,0,100) [ Scale (2,2) [ Ellipse (1.0,0.2) ] ]
                ] ]
        ]
    ]

-- A polygon roughly in the shape of the letter N
polygon :: Drawing
polygon = Polygon [(0.8,0.8),(-0.2,0.0),(-0.8,0.8),(-0.6,-0.8),(0.2,-0.3),(0.6,-0.8)]

-- Image C contains polygons
imageC :: [Drawing]
imageC = [
        Square,
        Scale (0.5,0.5) [
            Translate (-0.5, 0.75) [ Colour red [ polygon ] ],
            Rotate (pi/2) [ Translate (-1.0,-0.5) [ Colour blue [ polygon ] ] ]
        ]
    ]

imageDrawing :: Text -> ByteString
imageDrawing "a" = render imageA (500,500)
imageDrawing "b" = render imageB (300,300)
imageDrawing "c" = render imageC (500,300)
imageDrawing _ = ""

imageText :: Text -> Text
imageText "a" = pack $ show imageA
imageText "b" = pack $ show imageB
imageText "c" = pack $ show imageC
imageText _ = "Image not found"

main :: IO ()
main = S.scotty 8080 $ do
    -- Root of website
    S.get "/" $ do
        S.html $ renderHtml $ body $ do
            h2 "Project 1 - Image Server"
            a ! href "/a" $ "Image A - Colours, Shapes and Transformations" ; br
            a ! href "/b" $ "Image B - Image Masking" ; br
            a ! href "/c" $ "Image C - Arbitrary Polygons" ; br
    -- Image paths
    S.get "/:i" $ do
        i <- S.param "i"
        S.html $ renderHtml $ body $ do
            a ! href "/" $ "Back"
            h2 "eDSL Input"
            textarea ! rows "8" ! cols "80" $ toHtml $ imageText i
            h2 "Drawing Output"
            img ! src (toValue $ append "data:image/png;base64," $ encodeBase64 $ imageDrawing i)
