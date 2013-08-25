{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Pdf
-- Copyright   :  (c) 2013 alpheccar.org (see LICENSE)
-- License     :  BSD-style (see LICENSE)
--
-- A Pdf rendering backend for diagrams.
--
-- To build diagrams for Pdf rendering use the @Pdf@
-- type in the diagram type construction
--
-- > d :: Diagram Pdf R2
-- > d = ...
--
-- and render giving the @Pdf@ token
--
-- > renderDia Pdf (PdfOptions (Width 400)) d
--
-- This IO action will write the specified file.
--
-- / Specific HPDF primitives /
-- 
-- For details about the use of the HPDF specific primitives, the file
-- test.hs in this package can be used. You'll have to unpack the archive
-- for this package.
-----------------------------------------------------------------------------
module Diagrams.Backend.Pdf

  ( -- * PDF Backend 
    -- ** Backend token
    Pdf(..)
    -- ** Backend options
  , Options(..)
  , sizeFromSpec
  -- * HPDF Specific primitives 
  -- ** Text
  , LabelStyle(..)
  , TextOrigin(..)
  , LabelSize
  , pdfLabelWithSuggestedSize
  , pdfTextWithSuggestedSize
  , pdfLabelWithSize
  , pdfTextWithSize
  -- ** Image
  , pdfImage
  -- ** URL
  , pdfURL
  -- ** Shading
  , pdfAxialShading
  , pdfRadialShading
  ) where


import  Graphics.PDF hiding(transform,Style,translate,scale)
import qualified Graphics.PDF as P

import           Diagrams.Prelude

import           Diagrams.TwoD.Text

import           Data.Maybe                    (catMaybes)


import qualified Data.Foldable                 as F
import           Data.Monoid.Split
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans(lift)
import Diagrams.TwoD.Path
import Control.Monad(when)
import Diagrams.Backend.Pdf.Specific
import           Data.Typeable
import qualified Diagrams.TwoD.Shapes as Sh
import Data.Maybe(isJust)

--import Debug.Trace as T

--debug a = T.trace (show a) a 

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Pdf = Pdf
    deriving (Eq,Ord,Read,Show,Typeable)

-- | The drawing state
-- I should give a name to the different fields and use lens
-- The first three parameters are for the font
-- The P.Point is because the PDF specification is using absolute
-- coordinates but relative coordinates are needed for diagrams.
-- So the last point is tracked here.
-- The last bools are to disable fill and stroke.
-- Fill is disabled when transparency is total (color transparency and not diagram transparency)
-- Stroke is disabled when line width is 0 because in the PDF specification
-- line width 0 means the smallest width and something is displayed.
data DrawingState = DrawingState { _fontSlant :: FontSlant
                                 , _fontWeight :: FontWeight
                                 , _fontSize :: Int
                                 , _fillRule :: FillRule 
                                 , _currentPoint :: P.Point 
                                 , _mustFill :: Bool
                                 , _mustStroke :: Bool
                                 , _isloop :: Bool
                                 , _shading :: Maybe PDFShading
                               }

-- | The stack of drawing state
data StateStack = StateStack { _current :: DrawingState
                             , _last :: [DrawingState]
                             }

defaultFontSize :: Num a => a
defaultFontSize = 1 

diagramDefaultUnit :: Fractional a => a 
diagramDefaultUnit = 0.01 

defaultWidth :: Fractional a => a 
defaultWidth = 0.01

-- | Initial drawing state
initState :: StateStack
initState = StateStack (DrawingState FontSlantNormal FontWeightNormal 1 Winding (0 :+ 0) False True True Nothing) []


-- | The drawing monad with state
newtype DrawS a = DS (S.StateT StateStack Draw a) deriving(Monad,S.MonadState StateStack)

-- | List a Draw value into the DrawS monad
drawM :: Draw a -> DrawS a
drawM = DS . lift


-- | Get the MonadState wrapped in DS
unDS :: DrawS t -> S.StateT StateStack Draw t
unDS (DS r) = r 

-- | Get the Draw value from an initial state
runDS :: DrawS a -> Draw a
runDS d = S.evalStateT (unDS d) initState

-- | Generate an HPDF font
mkFont :: DrawingState -> PDFFont 
mkFont (DrawingState FontSlantNormal FontWeightNormal s _ _ _ _ _ _) = PDFFont Times_Roman s
mkFont (DrawingState FontSlantNormal FontWeightBold s _ _ _ _ _ _) = PDFFont Times_Bold s
mkFont (DrawingState FontSlantItalic FontWeightNormal s _ _ _ _ _ _) = PDFFont Times_Italic s
mkFont (DrawingState FontSlantItalic FontWeightBold s _ _ _ _ _ _) = PDFFont Times_BoldItalic s
mkFont (DrawingState FontSlantOblique FontWeightNormal s _ _ _ _ _ _) = PDFFont Helvetica_Oblique s
mkFont (DrawingState FontSlantOblique FontWeightBold s _ _ _ _ _ _) = PDFFont Helvetica_BoldOblique s



setFontSize :: Double -> DrawS ()
setFontSize fs = do 
  let s = floor fs
  StateStack (DrawingState fsl fw _ wr p f st ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState fsl fw s wr p f st ilp shade) l

setFontWeight :: FontWeight -> DrawS ()
setFontWeight w = do 
  StateStack (DrawingState fsl _ fs wr p f st ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState fsl w fs wr p f st ilp shade) l
  

setFontSlant :: FontSlant -> DrawS ()
setFontSlant sl = do 
  StateStack (DrawingState _ fw fs wr p f st ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp shade) l
  
setFillRule :: FillRule -> DrawS ()
setFillRule wr = do 
  StateStack (DrawingState sl fw fs _ p f st ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp shade) l

savePoint :: P.Point -> DrawS () 
savePoint p = do 
  StateStack (DrawingState sl fw fs wr _ f st ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp shade) l

currentPoint :: DrawS P.Point 
currentPoint = do 
  StateStack (DrawingState _ _ _ _ p _ _ _ _) _ <- S.get 
  return p 

getFillState :: DrawS FillRule 
getFillState = do 
  StateStack (DrawingState _ _ _ w _ _ _ _ _) _ <- S.get 
  return w

mustFill :: DrawS Bool 
mustFill = do
  StateStack (DrawingState _ _ _ _ _ b _ _ _) _ <- S.get 
  return b

-- | From the alpha value of a fill color, we check if the filling must be disabled
setFillingColor :: Double -> DrawS() 
setFillingColor alpha = do 
    let b | alpha /= 0.0 = True 
          | otherwise = False
    StateStack (DrawingState fsl w fs wr p _ st ilp shade) l <- S.get 
    S.put $! StateStack (DrawingState fsl w fs wr p b st ilp shade) l

mustStroke :: DrawS Bool
mustStroke = do 
  StateStack (DrawingState _ _ _ _ _ _ b _ _) _ <- S.get 
  return b

-- | From the linew width we check if stroke must be disabled
setTrokeState :: Double -> DrawS () 
setTrokeState w = do 
  let st | w == 0 = False 
         | otherwise = True
  StateStack (DrawingState fsl fw fs wr p b _ ilp shade) l <- S.get 
  S.put $! StateStack (DrawingState fsl fw fs wr p b st ilp shade) l

isALoop :: DrawS Bool 
isALoop = do 
  StateStack s _ <- S.get
  return $ _isloop s

getShading :: DrawS (Maybe PDFShading) 
getShading = do 
  StateStack s _ <- S.get
  return $ _shading $  s

setLoop :: Bool -> DrawS () 
setLoop b = do 
  StateStack s l <- S.get
  S.put $! StateStack (s {_isloop = b}) l

-- | Initial settings before rendering the diagram
setTranform :: Draw () -> Draw ()
setTranform d = do
     P.fillColor P.white 
     P.strokeColor P.black
     P.setWidth defaultWidth
     d

withShading :: Transformation R2 -> Bool -> Maybe PDFShading -> DrawS () -> DrawS () -> DrawS () 
withShading td evenodd (Just shade) diag _ = do 
    withContext $ do 
      diag
      drawM $ do
         if evenodd 
         then P.setAsClipPathEO
         else P.setAsClipPath
         P.applyShading (unTrans $ transform td (TransSh shade))
withShading _ _ _ diag paint = do 
  diag 
  paint

strokeOrFill :: Transformation R2 -> DrawS () -> DrawS () 
strokeOrFill td r = do 
  mf <- mustFill
  ms <- mustStroke 
  fs <- getFillState
  isloop <- isALoop
  sh <- getShading
  -- Set the diagram opacity in a new PDF context
  case (ms,mf,fs,isloop) of 
       (True,True,Winding,True) -> withShading td False sh r $ drawM (P.fillAndStrokePath)
       (True,True,EvenOdd,True) -> withShading td True sh r $ drawM (P.fillAndStrokePathEO)
       (False,True,Winding,True) -> withShading td False sh r $ drawM (P.fillPath)
       (False,True,EvenOdd,True) -> withShading td True sh r $ drawM (P.fillPathEO)
       (True,_,_,_) -> r >> drawM (P.strokePath) 
       (_,_,_,_) -> r >> return ()
  setLoop True

-- | Perform a rendering operation with a local style.
withStyle'     :: Style R2    -- ^ Style to use
               -> Transformation R2  -- ^ Transformation to be applied to the style
               -> Transformation R2 -- ^ Transformation that was applied to the diagram
               -> Render Pdf R2 -- ^ Rendering operation to run
               -> Render Pdf R2 -- ^ Rendering operation using the style locally
withStyle' s t td (D r) = D $ do
    withContext $ do
       pdfMiscStyle s
       mf <- mustFill
       ms <- mustStroke 
       -- Set the clip region into a new PDF context
       -- since it is the only way to restore the old clip region
       -- (by popping the PDF stack of contexts)
       pdfTransf t
       withClip s $ do
          pdfFrozenStyle s
          when (mf || ms) $ do 
            withPdfOpacity s $ do
               strokeOrFill (td) r

instance Backend Pdf R2 where
  data Render  Pdf R2 = D (DrawS ())
  type Result  Pdf R2 = Draw ()
  data Options Pdf R2 = PdfOptions {
      pdfsizeSpec :: SizeSpec2D
    } deriving(Show)
     
  -- There is something I don't understand here with the frozen style.
  -- On the tests it is working but I would not have put
  -- the calls in this order ... so it must be checked later   
  withStyle _ s t (D r) = withStyle' s t mempty (D r)

  doRender _ _ (D r) = setTranform (runDS r)

  renderDia Pdf opts d =
    centerAndScale d' . doRender Pdf opts' . mconcat . map renderOne . prims $ d'
      where (opts', d') = adjustDia Pdf opts d
            renderOne :: (Prim Pdf R2, (Split (Transformation R2), Style R2))
                      -> Render Pdf R2
            renderOne (p, (M t,      s))
              = withStyle' s mempty t (render Pdf (transform t p))

            renderOne (p, (t1 :| t2, s))
              -- Here is the difference from the default
              -- implementation: "t2" instead of "t1 <> t2".
              = withStyle' s t1 t2 (render Pdf (transform t2 p))
            centerAndScale diag renderedDiagram  = do
                let bd = boundingBox diag
                    (w,h) = sizeFromSpec (pdfsizeSpec opts)
                    rescaledD (Just (ll,ur)) =
                                let (vx,vy) = unp2 $ centroid [ll,ur]
                                    (xa,ya) = unp2 ll 
                                    (xb,yb) = unp2 ur 
                                    --ps = max (abs (xb - xa)) (abs (yb - ya))
                                    sx = w / (abs (xb - xa))
                                    sy = h / abs (yb - ya)
                                    s = min sx sy
                                    pageCenter = (w / 2.0) P.:+ (h/2.0)
                                in
                                do
                                  P.applyMatrix (P.translate pageCenter) 
                                  P.applyMatrix (P.scale s s)
                                  P.applyMatrix (P.translate $ (-vx) P.:+ (-vy))
                    rescaledD Nothing = return ()
                rescaledD (getCorners bd)
                P.withNewContext $ do
                  renderedDiagram

instance Monoid (Render Pdf R2) where
  mempty  = D (return ())
  (D a) `mappend` (D b) = D (a >> b)

sizeFromSpec :: SizeSpec2D -> (Double,Double)    
sizeFromSpec size = case size of
   Width w'   -> (w',w')
   Height h'  -> (h',h')
   Dims w' h' -> (w',h')
   Absolute   -> (200,200)

-- | Relative lineto
relativeLine :: P.Point -> DrawS ()
relativeLine p = do 
  c <- currentPoint 
  let c' = p + c
  drawM (lineto c')
  savePoint c'

-- | Relative curveto
relativeCurveTo :: P.Point -> P.Point -> P.Point -> DrawS () 
relativeCurveTo x y z = do 
  c <- currentPoint 
  let x' = x + c 
      y' = y + c 
      z' = z + c 
  drawM (curveto x' y' z')
  savePoint z'

-- | moveto but with saving of the point
moveToAndSave :: P.Point -> DrawS () 
moveToAndSave p = do 
  drawM (moveto p)
  savePoint p

-- | Convenience functions
renderC :: (Renderable a Pdf, V a ~ R2) => a -> DrawS ()
renderC a = case render Pdf a of D r -> r

{-
push :: DrawS()
push = do 
  StateStack c l <- S.get 
  S.put $! (StateStack c (c:l))

pop :: DrawS()
pop = do 
  StateStack _ l <- S.get 
  S.put $! (StateStack (head l) (tail l))
-}

-- | With a new context do something
-- It is a bit comlex because the withNewContext from the HPDF library
-- must be used to push / pop a new PDF context
withContext :: DrawS a -> DrawS a
withContext d = do 
  s <- S.get
  let d' = S.evalStateT (unDS d) s
  a <- drawM (withNewContext d') 
  return a



pdfFillColor :: (Real b, Floating b) => AlphaColour b -> DrawS ()
pdfFillColor c = do
  let (r,g,b,a) = colorToSRGBA c
  drawM $ do
      P.setFillAlpha a
      P.fillColor (Rgb r g b)
  setFillingColor a

pdfStrokeColor :: (Real b, Floating b) => AlphaColour b -> DrawS ()
pdfStrokeColor c = do
  drawM $ do
     let (r,g,b,a) = colorToSRGBA c
     P.setStrokeAlpha a
     P.strokeColor (Rgb r g b)

setShadingData :: Maybe PDFShading -> DrawS () 
setShadingData sh = do 
  StateStack s l <- S.get
  S.put $! StateStack (s {_shading = sh, _mustFill = _mustFill s || isJust sh}) l

setShading :: PdfShadingData -> DrawS ()
setShading (PdfAxialShadingData pa pb ca cb) = do 
  let (ra,ga,ba,_) = colorToSRGBA ca
      (rb,gb,bb,_) = colorToSRGBA cb 
      colora = Rgb ra ga ba 
      colorb = Rgb rb gb bb
      (xa,ya) = unp2 pa 
      (xb,yb) = unp2 pb 
  setShadingData $ Just (AxialShading xa ya xb yb colora colorb)
setShading (PdfRadialShadingData pa radiusa pb radiusb ca cb) = do 
  let (ra,ga,ba,_) = colorToSRGBA ca
      (rb,gb,bb,_) = colorToSRGBA cb 
      colora = Rgb ra ga ba 
      colorb = Rgb rb gb bb
      (xa,ya) = unp2 pa 
      (xb,yb) = unp2 pb 
  setShadingData $ Just $ RadialShading xa ya radiusa xb yb radiusb colora colorb
{-

Conversions between diagrams and HPDF types
  
-}

pdfLineJoin :: LineJoin -> P.JoinStyle
pdfLineJoin LineJoinMiter = MiterJoin
pdfLineJoin LineJoinRound = RoundJoin
pdfLineJoin LineJoinBevel = BevelJoin

pdfLineCap :: LineCap -> P.CapStyle 
pdfLineCap LineCapButt = ButtCap 
pdfLineCap LineCapRound = RoundCap
pdfLineCap LineCapSquare = SquareCap

pdfDashing :: Dashing -> DashPattern 
pdfDashing (Dashing l a) = DashPattern (map convert l) (convert a)
  where 
    convert x = defaultWidth * x / diagramDefaultUnit

{-

Opacity and clip attributes must be handled separately from the other
attributes

-}

withPdfOpacity :: Style v -> DrawS a -> DrawS a 
withPdfOpacity s m = do 
     let mo = handle s getOpacity
     case mo of 
      Nothing -> m 
      Just d -> do 
        withContext $ do 
          drawM (setStrokeAlpha d >> setFillAlpha d) 
          m

  where handle :: AttributeClass a => Style v -> (a -> b) -> Maybe b
        handle st f = f `fmap` getAttr st

withClip :: Style v -> DrawS () -> DrawS ()
withClip s m = do 
     let d = handle s m clip
     case d of 
       Just r -> r 
       Nothing -> m

  where handle :: AttributeClass a => Style v -> DrawS () -> (DrawS () -> a -> b) -> Maybe b
        handle st dm f = f dm `fmap` getAttr st
        clip dm = clipPath dm . getClip
        addPathToClip p = do 
          renderC p 
          f <- getFillState 
          case f of 
               Winding -> drawM (setAsClipPath)
               EvenOdd -> drawM (setAsClipPathEO)
        clipPath dm p = do 
          withContext $ do 
            mapM_ addPathToClip p 
            dm


pdfMiscStyle :: Style v -> DrawS ()
pdfMiscStyle s = do
  sequence_ . catMaybes $ [ handle fSlant
                          , handle fWeight
                          , handle fSize
                          , handle fColor
                          , handle lColor
                          , handle lFillRule
                          , handle checklWidth
                          , handle shading
                          ]
  where handle :: AttributeClass a => (a -> DrawS ()) -> Maybe (DrawS ())
        handle f = f `fmap` getAttr s
        fSize    = setFontSize . getFontSize
        --fFace    = const (return ())
        fSlant   = setFontSlant  . getFontSlant
        fWeight  = setFontWeight . getFontWeight
        lColor c = pdfStrokeColor . toAlphaColour . getLineColor $ c
        fColor c = pdfFillColor . toAlphaColour . getFillColor $ c
        lFillRule = setFillRule . getFillRule
        shading = setShading . getShadingData
        checklWidth w = do 
          let d = getLineWidth w
          setTrokeState d

pdfFrozenStyle :: Style v -> DrawS ()
pdfFrozenStyle s = sequence_ -- foldr (>>) (return ())
              . catMaybes $ [ handle lWidth
                            , handle lJoin
                            , handle lCap
                            , handle lDashing
                            ]
  where handle :: (AttributeClass a) => (a -> DrawS ()) -> Maybe (DrawS ())
        handle f = f `fmap` getAttr s
        lWidth w = do 
          let d = getLineWidth w
          drawM . setWidth $ (defaultWidth * d / diagramDefaultUnit)
          setTrokeState d
        lCap = drawM . setLineCap . pdfLineCap . getLineCap
        lJoin = drawM . setLineJoin . pdfLineJoin . getLineJoin
        lDashing = drawM . setDash . pdfDashing . getDashing
       
unR :: R2 -> Complex Double
unR r = let (x,y) = unr2 r
 in (x :+ y)

unP :: P2 -> Complex Double
unP r = let (x,y) = unp2 r
 in (x :+ y)

--showTrans :: Transformation R2 -> String 
--showTrans t = show a1 ++ " " ++ show b1 ++ " " ++ show c1 ++ "\n" ++
--              show a2 ++ " " ++ show b2 ++ " " ++ show c2 ++ "\n"
--   where (a1,a2) = unr2 $ apply t unitX
--         (b1,b2) = unr2 $ apply t unitY
--         (c1,c2) = unr2 $ transl t

--instance Show (Transformation R2) where 
--  show = showTrans

pdfTransf :: Transformation R2 -> DrawS ()
pdfTransf t = drawM $ applyMatrix (Matrix a1 a2 b1 b2 c1 c2)
  where (a1,a2) = unr2 $ apply t unitX
        (b1,b2) = unr2 $ apply t unitY
        (c1,c2) = unr2 $ transl t

instance Renderable (Segment Closed R2) Pdf where
  render _ (Linear (OffsetClosed (unR -> v))) = D $ relativeLine v
  render _ (Cubic (unR -> po1)
                  (unR -> po2)
                  (OffsetClosed (unR -> po3)))
    = D $ relativeCurveTo po1 po2 po3

instance Renderable (Trail R2) Pdf where
  render _ t = D . flip withLine t $ renderT . lineSegments
    where
      renderT segs =
        do
          mapM_ renderC segs
          when (isLoop t) (drawM closePath)
          setLoop (isLoop t)

instance Renderable (Path R2) Pdf where
  render _ (Path t) = D $ do
    F.mapM_ renderTrail t
    where renderTrail (viewLoc -> (unP -> p, tr)) = do
            moveToAndSave p
            renderC tr

instance Renderable Text Pdf where
  render _ (Text tr al str) = 
      D $ withContext $ do
            StateStack f _ <- S.get 
            let theFont = mkFont f
                tw = textWidth theFont (toPDFString str) 
                descent = getDescent theFont 
                fontHeight = getHeight theFont 
                (x,y) = case al of
                    BoxAlignedText xt yt -> (xt,yt)
                    BaselineText         -> (0,0)
                x' = - tw * x 
                y' = - (fontHeight - descent) * y
            pdfTransf tr
            withContext . drawM $ do
              P.applyMatrix (P.scale (1.0 / defaultFontSize) (1.0 / defaultFontSize))
              P.applyMatrix (P.translate (x' :+ y'))
              P.drawText $ P.text theFont 0 0 (toPDFString str)
            
{-

Rendering of specific HPDF primitives

-}

instance Renderable PdfTextBox Pdf where
  render _ (PdfTextBox t w h para) = D $ do
    withContext $ do
       pdfTransf t
       drawM (drawStringLabel w h para)

-- | Typeset a text with a given style in a suggested box.
-- The function is returning a diagram for the typeset text
-- and a diagram for the bounding box which may be smaller
-- than the suggested size : smaller width when the algorithm
-- has done some line justification. 
-- The text may also be bigger than the suggested width in case
-- of overflow (similar to the way TeX is doing thing. There are
-- settings in HPDF to control the elegance of the line cuts but
-- those settings are not accessible from this simple API).
-- The text will not be longer than the suggested height. In that
-- case the additional text is not displayed.
genericPdfText :: (Renderable PdfTextBox Pdf,Renderable (Path R2) Pdf) 
               => Bool -- ^ Suggested size 
               -> TextOrigin
               -> Double -- ^ Suggested width
               -> Double -- ^ Suggested height
               -> AnyFormattedParagraph
               -> (Diagram Pdf R2,Diagram Pdf R2) -- ^ Text and bounding rect of the typeset text
genericPdfText suggested o w h formatted = 
    let diag = mkQD (Prim (PdfTextBox mempty w h formatted))
                    (getEnvelope r)
                    (getTrace r)
                    mempty
                    (Query $ \p -> Any (isInsideEvenOdd p r))
        f v = (moveOriginTo v diag, moveOriginTo v textBounds)
    in
    case o of 
            LeftSide -> f east 
                  where 
                    east = p2 (0,-hlinewrap / 2.0)
            RightSide -> f west 
                  where 
                    west = p2 (wlinewrap,-hlinewrap / 2.0)
            Center -> f theCenter
                  where 
                    theCenter = p2 (wlinewrap / 2.0,-hlinewrap / 2.0)
            TopSide -> f topSide 
                  where 
                    topSide = p2 (wlinewrap / 2.0,0)
            BottomSide -> f bottomSide 
                  where 
                    bottomSide = p2 (wlinewrap / 2.0,-hlinewrap)
            TopLeftCorner -> f topLeft 
                  where 
                    topLeft = p2 (0,0)
            BottomLeftCorner -> f bottomLeft
                  where 
                    bottomLeft = p2 (0,-hlinewrap )
            TopRightCorner -> f topRight
                  where 
                    topRight = p2 (wlinewrap,0)
            BottomRightCorner -> f bottomRight
                  where 
                    bottomRight = p2 (wlinewrap,-hlinewrap )
        
  where wlinewrap :: Double 
        hlinewrap :: Double
        Rectangle (xa :+ ya) (xb :+ yb) | suggested = matchingContainerSize w h formatted
                                        | otherwise = Rectangle (0 :+ 0) (w :+ h)
        wlinewrap = xb - xa
        hlinewrap = yb - ya
   
        r :: Path R2
        r = rect wlinewrap hlinewrap # moveOriginTo (p2 (-wlinewrap / 2.0,hlinewrap / 2.0))
        textBounds :: Diagram Pdf R2
        textBounds = Sh.rect wlinewrap hlinewrap # moveOriginTo (p2 (-wlinewrap / 2.0,hlinewrap / 2.0))

-- | Typeset a text with a given style in a suggested box.
-- The function is returning a diagram for the text
-- and a diagram for the bounding box which may be smaller
-- than the suggested size : smaller width when the algorithm
-- has done some line justification. 
-- The text may also be bigger than the suggested width in case
-- of overflow (similar to the way TeX is doing thing. There are
-- settings in HPDF to control the elegance of the line cuts but
-- those settings are not accessible from this simple API).
-- The text will not be longer than the suggested height. In that
-- case the additional text is not displayed except perhaps partially the last
-- line since no clipping is done.
pdfLabelWithSuggestedSize :: (Renderable PdfTextBox Pdf,Renderable (Path R2) Pdf) 
                          => LabelStyle -- ^ Style of the label
                          -> String -- ^ String to display with this style
                          -> Double -- ^ Suggested width
                          -> Double -- ^ Suggested height
                          -> (Diagram Pdf R2,Diagram Pdf R2) -- ^ Text and bounding rect of the typeset text
pdfLabelWithSuggestedSize (LabelStyle fn fs j o fillc) s w h = 
  let pdfColor (r,g,b,_) = P.Rgb r g b
      pdffc = pdfColor . colorToSRGBA . toAlphaColour $ fillc 
  in
  genericPdfText True o w h $ (AFP NormalParagraph (P.Font (PDFFont fn fs) pdffc pdffc) $ do 
    setJustification j
    paragraph $ do
        txt $ s)

-- | Similar to the @pdfLabelWithSuggestedSize@ but supporting the full features of HPDF
pdfTextWithSuggestedSize :: (ParagraphStyle ps s, P.Style s,Renderable PdfTextBox Pdf,Renderable (Path R2) Pdf) 
                         => TextOrigin -- ^ Text origin
                         -> Double -- ^ Suggested width
                         -> Double -- ^ Suggested height
                         -> ps -- ^ Paragraph (vertical) style
                         -> s  -- ^ Horizontal style
                         -> TM ps s () -- ^ Text
                         -> (Diagram Pdf R2,Diagram Pdf R2) -- ^ Text and bounding rect of the typeset text
pdfTextWithSuggestedSize o w h ps hs tm = genericPdfText True o w h (AFP ps hs tm)

-- | Similar to the @pdfLabelWithSuggestedSize@ but here the size is forced and even
-- if the bounding box of the text is smaller it will not be taken into account
-- for the diagram envelope.
pdfLabelWithSize :: (Renderable PdfTextBox Pdf,Renderable (Path R2) Pdf) 
                 => LabelStyle -- ^ Style of the label
                 -> String -- ^ String to display with this style
                 -> Double -- ^ Suggested width
                 -> Double -- ^ Suggested height
                 -> Diagram Pdf R2 -- ^ Text
pdfLabelWithSize (LabelStyle fn fs j o fillc) s w h = 
  let pdfColor (r,g,b,_) = P.Rgb r g b
      pdffc = pdfColor . colorToSRGBA . toAlphaColour $ fillc 
  in
  fst $ genericPdfText False o w h $ (AFP NormalParagraph (P.Font (PDFFont fn fs) pdffc pdffc) $ do 
    setJustification j
    paragraph $ do
        txt $ s)

-- | Similar to @pdfTextWithSuggestedSize@ but the size if forced and not just suggested
pdfTextWithSize :: (ParagraphStyle ps s, P.Style s,Renderable PdfTextBox Pdf,Renderable (Path R2) Pdf) 
                => TextOrigin -- ^ Text origin
                -> Double -- ^ Suggested width
                -> Double -- ^ Suggested height
                -> ps -- ^ Paragraph (vertical) style
                -> s  -- ^ Horizontal style
                -> TM ps s () -- ^ Text
                -> Diagram Pdf R2 -- ^ Text
pdfTextWithSize o w h ps hs tm = fst $ genericPdfText False o w h (AFP ps hs tm)

instance Renderable PdfImage Pdf where
  render _ (PdfImage t ref) = D $ do
    withContext $ do
       pdfTransf t
       drawM . drawXObject $ ref 

-- | Create an image diagram
pdfImage :: (Monad m, PDFGlobals m)
         => PDFReference PDFJpeg -- ^ Reference to the Jpeg image in the PDF resources
         -> m (Diagram Pdf R2)
pdfImage ref = do 
    (w,h) <- P.bounds ref
    let r :: Path R2
        r = rect w h # moveOriginTo (p2 (-w/2, h/2.0))
        diag = mkQD (Prim (PdfImage mempty ref))
                    (getEnvelope r)
                    (getTrace r)
                    mempty
                    (Query $ \p -> Any (isInsideEvenOdd p r))
    return (diag # moveOriginTo (p2 (w/2.0,h/2.0)))

instance Renderable PdfURL Pdf where
  render _ (PdfURL t url w h) = D $ do
    withContext $ do
       pdfTransf t
       drawM $ do 
        newAnnotation (URLLink (toPDFString "diagrams link") [0,0,w,h] url True)

-- | Create an URL diagram
pdfURL :: String -- ^ URL (in a next version it should be URI)
       -> Double -- ^ Width of active area
       -> Double -- ^ Height of active area
       -> Diagram Pdf R2 
pdfURL url w h = 
  let r = rect w h # moveOriginTo (p2 (-w/2, h/2.0))
      diag = mkQD (Prim (PdfURL mempty url w h))
                    (getEnvelope r)
                    (getTrace r)
                    mempty
                    (Query $ \p -> Any (isInsideEvenOdd p r))
  in 
  diag # moveOriginTo (p2 (w/2.0,h/2.0))


-- To avoid an Orphan instance warning for Transformable PDFShading
newtype TransSh =TransSh {unTrans :: PDFShading}

type instance V TransSh = R2

instance Transformable TransSh where 
    transform t (TransSh (AxialShading xa ya xb yb ca cb)) = TransSh $ AxialShading xa' ya' xb' yb' ca cb 
      where 
        (xa',ya') = unp2 . transform t $ p2 (xa,ya)
        (xb',yb') = unp2 . transform t $ p2 (xb,yb)
    transform t (TransSh (RadialShading xa ya ra xb yb rb ca cb)) = TransSh $ RadialShading xa' ya' ra xb' yb' rb ca cb 
      where 
        (xa',ya') = unp2 . transform t $ p2 (xa,ya)
        (xb',yb') = unp2 . transform t $ p2 (xb,yb)
       
{-

Paragraph shapes 

-}

{-    
data EnvelopedPara v = EnvelopedPara (Envelope R2) v

instance ComparableStyle v => ComparableStyle (EnvelopedPara v) where 
  isSameStyleAs (EnvelopedPara _ va) (EnvelopedPara _ vb) = isSameStyleAs va vb 

instance (ComparableStyle v,P.Style s, ParagraphStyle v s) => ParagraphStyle (EnvelopedPara v) s where 
   linePosition (EnvelopedPara _ v) = P.linePosition v
   lineWidth (EnvelopedPara _ v) = P.lineWidth v
   interline (EnvelopedPara _ v) = P.interline v
   paragraphChange (EnvelopedPara a v) i l = let (np,r) = P.paragraphChange v i l in (EnvelopedPara a np,r)
   paragraphStyle (EnvelopedPara _ v) = P.paragraphStyle v

-}