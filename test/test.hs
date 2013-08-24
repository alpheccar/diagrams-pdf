{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Pdf
import Diagrams.Backend.Pdf.CmdLine
import Data.Colour (withOpacity)
import Graphics.PDF hiding(scale,red,green,blue,text,rotate)
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Diagrams.Example.Logo as L
import           Diagrams.Coordinates ((&))

diag = mconcat [ circle 0.1 # fc green
               , triangle 1 # scale 0.4 # fc yellow
               , square 1   # fc blue
               , circle 1   # fc red
               ]


example = (square 1
      ||| square 1 # freeze # scale 2
      ||| circle 1 # freeze # scaleX 3)  # lw 0.03

example2 = (square 1
      ||| square 1 # scale 2
      ||| circle 1 # scaleX 3)   # lw 0.03

eff = text "F" <> square 1 # lw 0.1
rs  = map rotateBy [1/7, 2/7 .. 6/7]
example3 = hcat (replicate 5 eff) --hcat . map (eff #) $ rs

s c     = square 1 # fc c
reds    = (s darkred ||| s red) === (s pink ||| s indianred)
example4 = (hcat' with { sep = 1 } . take 4 . iterate (opacity 0.7) $ reds)

--example5 = pdfText style1 "This is a test" 100 100 <> square 40
-- where 
--   style1 = LabelStyle 12 Centered N

pt = circle 0.1 # fc red

t1 = pt <> topLeftText         "top left"   <> rect 8 1
t2 = pt <> baselineText        "baseline"   <> rect 8 1
t3 = pt <> alignedText 0.7 0.5 "(0.7, 0.5)" # fc red <> rect 8 1
t4 = pt <> text "Hello World !"  <> rect 8 1

d1 =/= d2 = d1 === strutY 2 === d2
testtext = t1 =/= t2 =/= t3 =/= t4

eff1 = text "F" <> square 1 # lw 0
ts  = [ scale (1/2), id, scale 2,    scaleX 2,    scaleY 2
      ,                  scale (-1), scaleX (-1), scaleY (-1)
      ]

example6 = hcat . map (eff1 #) $ ts

testatt = 
    let path = fromVertices (map p2 [(0,0), (1,0.3), (2,0), (2.2,0.3)]) # lw 0.1
    in pad 1.1 . centerXY . vcat' with { sep = 0.1 }
              $ map (path #)
                [ lineCap LineCapButt   . lineJoin LineJoinMiter
                , lineCap LineCapRound  . lineJoin LineJoinRound
                , lineCap LineCapSquare . lineJoin LineJoinBevel
                , dashing [0.1,0.2,0.3,0.1] 0
                ]

b1 = square 20 # lw 0.002

t s x j = 
    let (td, rd) = pdfLabelWithSuggestedSize (LabelStyle Times_Roman 12 j x blue) s 50 100 
    in 
    td # showOrigin # lw 0.03  <> rd

testpdftext = pad 1.1 . centerXY $ (centerXY squareText) <> square 200
 where 
  simple = t "Top Left" BottomLeftCorner LeftJustification ||| square 50
  squareText = (t "Top Left" TopLeftCorner LeftJustification ||| t "Top" TopSide Centered ||| t "Top Right" TopRightCorner RightJustification)
               ===
               (t "Left" LeftSide LeftJustification ||| t "Center" Center Centered ||| t "Right" RightSide RightJustification)
               ===
               (t "Bottom Left" BottomLeftCorner LeftJustification ||| t "Bottom" BottomSide Centered ||| t "Bottom Right" BottomRightCorner RightJustification)
  
        
  pt = circle 0.1 # fc red
  t1 = pt <> t "Top Left" TopLeftCorner LeftJustification  <> rect 100 50

testPict p = ((p # showOrigin <> rect 60 57) ||| (square 100 # lc blue)) <> rect 600 400 <> pdfURL "http://www.alpheccar.org" 100 100
  <> t "Top Left" TopLeftCorner LeftJustification

testPict2 :: Diagram Pdf R2 -> Diagram Pdf R2
testPict2 p = ((p # showOrigin <> pdfURL "http://www.alpheccar.org" 60 57) ||| square 100 # showOrigin # lc blue) <> rect 600 400

testShading :: Diagram Pdf R2
testShading = square 100 # pdfAxialShading (p2 (-50,-50)) (p2 (50,50)) blue red  <> rect 600 400

testRadialShading :: Diagram Pdf R2
testRadialShading = square 100 # pdfRadialShading (p2 (0,0)) 20 (p2 (0,0)) 50 blue red  <> rect 600 400

main = do
  Right jpgf <- readJpegFile "logo.jpg" 
  let rect = PDFRect 0 0 600 400
  runPdf "circle.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
      --jpg <- createPDFJpeg jpgf
      page1 <- addPage Nothing
      drawWithPage page1 $ do
        --p <- pdfImage jpg
        renderDia Pdf (PdfOptions (Dims 600 400)) $ (example4 :: Diagram Pdf R2)
          --testpdftext
