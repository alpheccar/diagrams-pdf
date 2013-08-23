{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Pdf
import Diagrams.Backend.Pdf.CmdLine
import Data.Colour (withOpacity)
import Graphics.PDF(Orientation(..), Justification(..),FontName(..))
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

testpdftext = pad 1.1 . centerXY $ (centerXY squareText) <> square 200
 where 
  simple = t "Top Left" BottomLeftCorner LeftJustification ||| square 50
  squareText = (t "Top Left" TopLeftCorner LeftJustification ||| t "Top" TopSide Centered ||| t "Top Right" TopRightCorner RightJustification)
               ===
               (t "Left" LeftSide LeftJustification ||| t "Center" Center Centered ||| t "Right" RightSide RightJustification)
               ===
               (t "Bottom Left" BottomLeftCorner LeftJustification ||| t "Bottom" BottomSide Centered ||| t "Bottom Right" BottomRightCorner RightJustification)
  t s x j = 
    let (td, rd) = pdfLabelWithSuggestedSize (LabelStyle Times_Roman 12 j x (SomeColor blue)) s 50 100 
    in 
    td # showOrigin # lw 0.03  <> rd
        
  pt = circle 0.1 # fc red
  t1 = pt <> t "Top Left" TopLeftCorner LeftJustification  <> rect 100 50

main = do
	-- multipleMain $ [testatt,L.logo,pad 1.1 . centerXY $ example4, example2,example]
  defaultMain testpdftext
	--S.defaultMain (circle 1.0 # fc blue)