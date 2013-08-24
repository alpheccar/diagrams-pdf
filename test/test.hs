{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
import Diagrams.Prelude
import Diagrams.Backend.Pdf
import Diagrams.Backend.Pdf.CmdLine
import Data.Colour (withOpacity)
import Graphics.PDF hiding(scale,red,green,blue,white,black,text,rotate,rect)
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Diagrams.Example.Logo as L
import           Diagrams.Coordinates ((&))
import qualified Graphics.PDF.Typesetting as T
import qualified Graphics.PDF as P
import System.Random

data MyParaStyles = Normal
                  | Bold
                  | Crazy
                  | SuperCrazy [Int] [PDFFloat]
                  | DebugStyle
                  | RedRectStyle
                  | BlueStyle
                  
instance ComparableStyle MyParaStyles where
  isSameStyleAs Normal Normal = True
  isSameStyleAs Bold Bold = True
  isSameStyleAs Crazy Crazy = True
  isSameStyleAs (SuperCrazy _ _) (SuperCrazy _ _) = True
  isSameStyleAs DebugStyle DebugStyle = True
  isSameStyleAs RedRectStyle RedRectStyle = True
  isSameStyleAs BlueStyle BlueStyle = True
  isSameStyleAs _ _ = False
  
                  
instance T.Style MyParaStyles where
    textStyle Normal = TextStyle (PDFFont Times_Roman 10) P.black P.black FillText 1.0 1.0 1.0 1.0
    textStyle Bold = TextStyle (PDFFont Times_Bold 12) P.black P.black FillText 1.0 1.0 1.0 1.0
    textStyle RedRectStyle = TextStyle (PDFFont Times_Roman 10) P.black P.black FillText 1.0 1.0 1.0 1.0
    textStyle DebugStyle = TextStyle (PDFFont Times_Roman 10) P.black P.black FillText 1.0 1.0 1.0 1.0
    textStyle Crazy = TextStyle (PDFFont Times_Roman 10) P.red P.red FillText 1.0 1.0 1.0 1.0
    textStyle (SuperCrazy _ _) = TextStyle (PDFFont Times_Roman 12) P.black P.black FillText 1.0 2.0 0.5 0.5
    textStyle BlueStyle = TextStyle (PDFFont Times_Roman 10) P.black P.black FillText 1.0 1.0 1.0 1.0
    
    sentenceStyle BlueStyle = Just $ \r d -> do
        P.fillColor $ Rgb 0.6 0.6 1
        P.strokeColor $ Rgb 0.6 0.6 1
        P.fillAndStroke r
        d
        return()
    
    sentenceStyle RedRectStyle = Just $ \r d -> do
        P.strokeColor P.red
        P.stroke r
        d
        return()
    sentenceStyle Crazy = Just $ \r d -> do
       d
       P.strokeColor P.blue
       P.stroke r
    sentenceStyle _ = Nothing
           
    wordStyle DebugStyle = Just $ \r m d ->
      case m of
          DrawWord -> d >> return ()
          DrawGlue -> d >> P.stroke r
    wordStyle Crazy = Just crazyWord
    wordStyle (SuperCrazy l _) = Just ws 
     where
        ws _ DrawGlue _ = return ()
        ws (Rectangle (xa :+ ya) (xb :+ yb)) DrawWord drawWord = do
            let [a,b,c,d,e,f,g,h] :: [PDFFloat] = map (\x -> x / 16.0) . map fromIntegral . take 8 $ l
                --angle = head angl
                p = Polygon [ (xa-a) :+ (ya+b)
                            , (xb+c) :+ (ya+d)
                            , (xb+e) :+ (yb-f)
                            , (xa-g) :+ (yb-h)
                            , (xa-a) :+ (ya+b)
                            ]
            P.strokeColor P.red
            P.stroke p
            P.fillColor $ Rgb 0.8 1.0 0.8
            P.fill p
            withNewContext $ do
              --applyMatrix . rotate . Degree $ angle
              drawWord
            return ()

    wordStyle _ = Nothing
    
    updateStyle (SuperCrazy a b) = SuperCrazy (drop 8 a) (tail b)
    updateStyle a = a
    
    styleHeight r@(SuperCrazy _ _) = (getHeight . textFont . textStyle $ r) + 4.0
    styleHeight r = getHeight . textFont . textStyle $ r
    
    styleDescent r@(SuperCrazy _ _) = (getDescent . textFont . textStyle $ r) + 2
    styleDescent r = getDescent . textFont . textStyle $ r
    
crazyWord :: Rectangle -> StyleFunction -> Draw a -> Draw ()
crazyWord r@(Rectangle (xa :+ ya) (xb :+ yb)) DrawWord d = do
    P.fillColor $ Rgb 0.6 1 0.6 
    P.fill r
    d
    P.strokeColor $ Rgb 0 0 1
    let m = (ya+yb)/2.0
    P.stroke $ P.Line xa m xb m 
crazyWord (Rectangle (xa :+ ya) (xb :+ yb)) DrawGlue _ = do
    P.fillColor $ Rgb 0 0 1
    P.fill (Circle ((xa+xb)/2.0) ((ya+yb)/2.0) ((xb-xa)/2.0))
    
   
    
superCrazy :: MyParaStyles
superCrazy = SuperCrazy (randomRs (0,32) (mkStdGen 0)) (randomRs (-10.0,10.0) (mkStdGen 10000))
    
data MyVertStyles = NormalPara
                  | CirclePara
                  | BluePara !PDFFloat

instance ComparableStyle MyVertStyles where
    isSameStyleAs NormalPara NormalPara = True
    isSameStyleAs CirclePara CirclePara = True
    isSameStyleAs (BluePara _) (BluePara _) = True
    isSameStyleAs _ _ = False


instance ParagraphStyle MyVertStyles MyParaStyles  where
    lineWidth (BluePara a) w nb = (if nb > 3 then w else w-a) - 20.0
    lineWidth CirclePara _ nb = 
           let nbLines = 15.0
               r = nbLines * (getHeight . textFont . textStyle $ Normal)
               pasin x' = if x' >= 1.0 then pi/2 else if x' <= -1.0 then (-pi/2) else asin x'
               angle l = pasin $ (nbLines - (fromIntegral l)  ) / nbLines
           in
           abs(2*r*cos (angle nb))
    lineWidth _ w _ = w
           
    linePosition (BluePara a) _ nb = (if nb > 3 then 0.0 else a) + 10.0
    linePosition a@(CirclePara) w nb = max 0 ((w - P.lineWidth a w nb) / 2.0)
    linePosition _ _ _ = 0.0
    
    interline (BluePara _) = Just $ \r -> do
        P.fillColor $ Rgb 0.6 0.6 1
        P.strokeColor $ Rgb 0.6 0.6 1
        P.fillAndStroke r
    interline _ = Nothing
        
    paragraphChange (BluePara _) _ (AChar st c _:l) = 
        let f = PDFFont Helvetica_Bold 45
            w' = charWidth f c 
            charRect = Rectangle (0 :+ (- getDescent f)) (w' :+ (getHeight f - getDescent f))
            c' = mkLetter (0,0,0) Nothing . mkDrawBox $ do
                withNewContext $ do
                    applyMatrix $ P.translate ((-w') :+ (getDescent f - getHeight f + styleHeight st - styleDescent st))
                    P.fillColor $ Rgb 0.6 0.6 1
                    P.strokeColor $ Rgb 0.6 0.6 1
                    P.fillAndStroke $ charRect
                    P.fillColor P.black
                    drawText $ do
                        renderMode AddToClip
                        textStart 0 0
                        setFont f
                        displayText (toPDFString [c])
                    paintWithShading (AxialShading 0 (- getDescent f) w' (getHeight f - getDescent f) (Rgb 1 0 0) (Rgb 0 0 1)) (addShape charRect)
        in
        (BluePara w', c':l)
    
    paragraphChange s _ l = (s,l)
    
    paragraphStyle (BluePara _) = Just $ \(Rectangle (xa :+ ya) (xb :+ yb)) b -> do
        let f = Rectangle ((xa-3) :+ (ya-3)) ((xb+3) :+ (yb+3))
        P.fillColor $ Rgb 0.6 0.6 1
        P.fill f
        b
        P.strokeColor P.red
        P.stroke f
        return ()
    paragraphStyle _ = Nothing
 
standardStyleTest :: TM MyVertStyles MyParaStyles ()
standardStyleTest = do
    paragraph $ do
        txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut "
        txt $ "labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris "
        setStyle Crazy
        txt $ "nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate "
        txt $ "velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non "
        txt $ "proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
 

complexText = 
  centerXY (fst (pdfTextWithSuggestedSize Center 400 200 NormalPara Normal standardStyleTest) 
    ===
    strutY 40 
    ===
    fst (pdfTextWithSuggestedSize Center 300 200 NormalPara Normal standardStyleTest) # rotate (20 :: Deg)
  )
  <> rect 600 300

t s x j = 
    let (td, rd) = pdfLabelWithSuggestedSize (LabelStyle Times_Roman 12 j x blue) s 50 100 
    in 
    td # showOrigin # lw 0.03  <> rd

tfs s x j = 
    let td = pdfLabelWithSize (LabelStyle Times_Roman 12 j x blue) s 50 50 
    in 
    td # showOrigin # lw 0.03 

testpdfsuggestedtextsize = pad 1.1 . centerXY $ (centerXY squareText) <> rect 600 300
 where 
  squareText = (t "Top Left" TopLeftCorner LeftJustification ||| t "Top" TopSide Centered ||| t "Top Right" TopRightCorner RightJustification)
               ===
               (t "Left" LeftSide LeftJustification ||| t "Center" Center Centered ||| t "Right" RightSide RightJustification)
               ===
               (t "Bottom Left" BottomLeftCorner LeftJustification ||| t "Bottom" BottomSide Centered ||| t "Bottom Right" BottomRightCorner RightJustification)
  
testpdftextsize = pad 1.1 . centerXY $ (centerXY squareText) <> rect 600 300
 where 
  squareText = (tfs "Top Left" TopLeftCorner LeftJustification ||| tfs "Top" TopSide Centered ||| tfs "Top Right" TopRightCorner RightJustification)
               ===
               (tfs "Left" LeftSide LeftJustification ||| tfs "Center" Center Centered ||| tfs "Right" RightSide RightJustification)
               ===
               (tfs "Bottom Left" BottomLeftCorner LeftJustification ||| tfs "Bottom" BottomSide Centered ||| tfs "Bottom Right" BottomRightCorner RightJustification)
  

 
testShading = 
   let loopyStar = fc red
                 . mconcat . map (cubicSpline True)
                 . pathVertices
                 . star (StarSkip 3)
                 $ regPoly 7 1
       f z d = 
        let s = 20
        in
        loopyStar # scale s # fillRule z # pdfAxialShading (p2 (-1,-1)) (p2 (1,1)) red green # rotate (d :: Deg)
   in   centerXY (hcat (map (f Winding) [0,20,40,60,80,100,120,140,160]))
        === centerXY (hcat (map (f EvenOdd) [0,20,40,60,80,100,120,140,160]))
        === square 40 # pdfRadialShading (p2 (0,0)) 5 (p2 (0,0)) 40 blue red

testImage img = 
    let url = "http://www.alpheccar.org" 
    in 
       circle 100 
    <> mconcat (map (\r -> img # scale 0.5 # translateX 100 # rotate r) ([0,20..360] :: [Deg]))
    <> pdfURL url 100 20 
    <> fst (pdfLabelWithSuggestedSize (LabelStyle Times_Roman 12 Centered Center blue) url 150 40)

mkSection s sect = 
    let (d,_) = pdfLabelWithSuggestedSize (LabelStyle Times_Roman 36 Centered Center blue) s 600 400 
    in do
      page1 <- addPage Nothing
      drawWithPage page1 $ do
        renderDia Pdf (PdfOptions (Dims 600 400)) d
      sect

page s d = header s === content d 
 where 
  width = 600 
  height = 400 
  titleH = 50
  header s = 
    let (d,_) = pdfLabelWithSuggestedSize (LabelStyle Times_Roman 24 Centered Center black) s width titleH 
    in 
    d <> (rect width titleH # pdfAxialShading (p2 (-width/2,-titleH/2)) (p2 (width/2,titleH/2)) blue white) 
  content d = (rect width (height - titleH) # lw 0) <> (centerXY d)


mkPage :: String -> Diagram Pdf R2 -> PDF ()
mkPage s d = do 
  page1 <- addPage Nothing
  drawWithPage page1 $ do
        renderDia Pdf (PdfOptions (Dims 600 400)) $ page s d

main = do
  Right jpgf <- readJpegFile "logo.jpg" 
  let theDocRect = PDFRect 0 0 600 400
  runPdf "circle.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) theDocRect $ do
      mkSection "HPDF Specific Primitives" $ do
           jpg <- createPDFJpeg jpgf
           image <- pdfImage jpg
           mkPage "Test JPEG and URL" (testImage image)
           mkPage "Test Shading" testShading
           mkPage "Test Suggested Text Container Size" testpdfsuggestedtextsize
           mkPage "Test Forced Text Container Size" testpdftextsize
           mkPage "Text Complex Text" complexText

