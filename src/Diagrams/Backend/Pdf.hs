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
-----------------------------------------------------------------------------
module Diagrams.Backend.Pdf

  ( -- * Backend token
    Pdf(..)
  , Options(..)
  , sizeFromSpec
  ) where


import  Graphics.PDF hiding(transform,Style,translate,scale)
import qualified Graphics.PDF as P

import           Diagrams.Prelude

import           Diagrams.TwoD.Text

import           Data.Maybe                    (catMaybes)


import qualified Data.Foldable                 as F
import           Data.Monoid.Split
import           Data.Typeable
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans(lift)
import Diagrams.TwoD.Path
import Control.Monad(when)

-- | This data declaration is simply used as a token to distinguish this rendering engine.
data Pdf = Pdf
    deriving (Eq,Ord,Read,Show,Typeable)

{-
 
For a future release to support some specific HPDF features

-}
{-
data LabelStyle = LabelStyle Int Justification P.Orientation 

data TextBox = TextBox T2 Double Double LabelStyle String

type instance V TextBox = R2

instance Transformable TextBox where
  transform t (TextBox tt w h a s) = TextBox (t <> tt) w h a s

instance IsPrim TextBox

instance HasOrigin TextBox where
  moveOriginTo p = translate (origin .-. p)

instance Renderable TextBox NullBackend where
  render _ _ = mempty

pdfText :: (Renderable TextBox b) 
        => LabelStyle 
        -> String 
        -> Double 
        -> Double 
        -> Diagram b R2
pdfText ls s w h = mkQD (Prim (TextBox mempty w h ls s))
                         (getEnvelope r)
                         (getTrace r)
                         mempty
                         (Query $ \p -> Any (isInsideEvenOdd p r))

  where r :: Path R2
        r = rect w h

drawStringLabel :: LabelStyle 
                -> String 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> Draw () 
drawStringLabel (LabelStyle fs j o) s x y w h = do
  let (r,b) = drawTextBox x y w h o NormalParagraph (P.Font (PDFFont Times_Roman fs) P.black P.black) $ do
                setJustification j
                paragraph $ do
                    txt $ s
  b

instance Renderable TextBox Pdf where
  render _ (TextBox t w h ls text) = D $ do
    let r :: Path R2
        r = rect w h 
        r' = transform t r 
        b = boundingBox r' 
        corners = getCorners b 
    case corners of 
       Just (a,b) -> do 
        let (xa,ya) = unp2 a 
            (xb,yb) = unp2 b
        drawM $ P.stroke $ Rectangle (xa :+ ya) (xb :+ yb) 
        drawM (drawStringLabel ls text xa ya (xb-xa) (yb-ya)) 
       Nothing -> return() 
-}

{-
 
End of the specific part

-}

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
initState = StateStack (DrawingState FontSlantNormal FontWeightNormal 1 Winding (0 :+ 0) False True False) []


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
mkFont (DrawingState FontSlantNormal FontWeightNormal s _ _ _ _ _) = PDFFont Times_Roman s
mkFont (DrawingState FontSlantNormal FontWeightBold s _ _ _ _ _) = PDFFont Times_Bold s
mkFont (DrawingState FontSlantItalic FontWeightNormal s _ _ _ _ _) = PDFFont Times_Italic s
mkFont (DrawingState FontSlantItalic FontWeightBold s _ _ _ _ _) = PDFFont Times_BoldItalic s
mkFont (DrawingState FontSlantOblique FontWeightNormal s _ _ _ _ _) = PDFFont Helvetica_Oblique s
mkFont (DrawingState FontSlantOblique FontWeightBold s _ _ _ _ _) = PDFFont Helvetica_BoldOblique s



setFontSize :: Double -> DrawS ()
setFontSize fs = do 
  let s = floor fs
  StateStack (DrawingState fsl fw _ wr p f st ilp) l <- S.get 
  S.put $! StateStack (DrawingState fsl fw s wr p f st ilp) l

setFontWeight :: FontWeight -> DrawS ()
setFontWeight w = do 
  StateStack (DrawingState fsl _ fs wr p f st ilp) l <- S.get 
  S.put $! StateStack (DrawingState fsl w fs wr p f st ilp) l
  

setFontSlant :: FontSlant -> DrawS ()
setFontSlant sl = do 
  StateStack (DrawingState _ fw fs wr p f st ilp) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp) l
  
setFillRule :: FillRule -> DrawS ()
setFillRule wr = do 
  StateStack (DrawingState sl fw fs _ p f st ilp) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp) l

savePoint :: P.Point -> DrawS () 
savePoint p = do 
  StateStack (DrawingState sl fw fs wr _ f st ilp) l <- S.get 
  S.put $! StateStack (DrawingState sl fw fs wr p f st ilp) l

currentPoint :: DrawS P.Point 
currentPoint = do 
  StateStack (DrawingState _ _ _ _ p _ _ _) _ <- S.get 
  return p 

getFillState :: DrawS FillRule 
getFillState = do 
  StateStack (DrawingState _ _ _ w _ _ _ _) _ <- S.get 
  return w

mustFill :: DrawS Bool 
mustFill = do
  StateStack (DrawingState _ _ _ _ _ b _ _) _ <- S.get 
  return b

-- | From the alpha value of a fill color, we check if the filling must be disabled
setFillingColor :: Double -> DrawS() 
setFillingColor alpha = do 
    let b | alpha /= 0.0 = True 
          | otherwise = False
    StateStack (DrawingState fsl w fs wr p _ st ilp) l <- S.get 
    S.put $! StateStack (DrawingState fsl w fs wr p b st ilp) l

mustStroke :: DrawS Bool
mustStroke = do 
  StateStack (DrawingState _ _ _ _ _ _ b _) _ <- S.get 
  return b

-- | From the linew width we check if stroke must be disabled
setTrokeState :: Double -> DrawS () 
setTrokeState w = do 
  let st | w == 0 = False 
         | otherwise = True
  StateStack (DrawingState fsl fw fs wr p b _ ilp) l <- S.get 
  S.put $! StateStack (DrawingState fsl fw fs wr p b st ilp) l

isALoop :: DrawS Bool 
isALoop = do 
  StateStack s _ <- S.get
  return $ _isloop s

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

strokeOrFill :: DrawS () 
strokeOrFill = do 
  mf <- mustFill
  ms <- mustStroke 
  fs <- getFillState
  isloop <- isALoop
  -- Set the diagram opacity in a new PDF context
  case (ms,mf,fs,isloop) of 
       (True,True,Winding,True) -> drawM (P.fillAndStrokePath)
       (True,True,EvenOdd,True) -> drawM (P.fillAndStrokePathEO)
       (False,True,Winding,True) -> drawM (P.fillPath)
       (False,True,EvenOdd,True) -> drawM (P.fillPathEO)
       (True,_,_,_) -> drawM (P.strokePath) 
       (_,_,_,_) -> return ()
  setLoop True

instance Backend Pdf R2 where
  data Render  Pdf R2 = D (DrawS ())
  type Result  Pdf R2 = Draw ()
  data Options Pdf R2 = PdfOptions {
      pdfsizeSpec :: SizeSpec2D
    } deriving(Show)
     
  -- There is something I don't understand here with the frozen style.
  -- On the tests it is working but I would not have put
  -- the calls in this order ... so it must be checked later   
  withStyle _ s t (D r) = D $ do
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
               r
               strokeOrFill

  doRender _ _ (D r) = setTranform (runDS r)

  renderDia Pdf opts d =
    centerAndScale opts d . doRender Pdf opts' . mconcat . map renderOne . prims $ d'
      where (opts', d') = adjustDia Pdf opts d
            renderOne :: (Prim Pdf R2, (Split (Transformation R2), Style R2))
                      -> Render Pdf R2
            renderOne (p, (M t,      s))
              = withStyle Pdf s mempty (render Pdf (transform t p))

            renderOne (p, (t1 :| t2, s))
              -- Here is the difference from the default
              -- implementation: "t2" instead of "t1 <> t2".
              = withStyle Pdf s t1 (render Pdf (transform t2 p))
            centerAndScale opts diag renderedDiagram  = do
                let bd = boundingBox diag
                    (w,h) = sizeFromSpec (pdfsizeSpec opts)
                    rescaledD (Just (ll,ur)) =
                                let (vx,vy) = unp2 $ centroid [ll,ur]
                                    (xa,ya) = unp2 ll 
                                    (xb,yb) = unp2 ur 
                                    ps = max (abs (xb - xa)) (abs (yb - ya))
                                    sx = w / ps
                                    sy = h / ps
                                    pageCenter = (w / 2.0) P.:+ (h/2.0)
                                in
                                do
                                  P.applyMatrix (P.translate pageCenter) 
                                  P.applyMatrix (P.scale sx sy)
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
pdfStrokeColor c = drawM $ do
  let (r,g,b,a) = colorToSRGBA c
  P.setStrokeAlpha a
  P.strokeColor (Rgb r g b)

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
            

