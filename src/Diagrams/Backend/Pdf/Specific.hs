{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Diagrams.Backend.Pdf.Specific(
      LabelStyle(..)
    , PdfTextBox(..)
    , drawStringLabel
    , getTextBoundingBox
    , TextOrigin(..)
    , LabelSize
    , PdfImage(..)
    , PdfURL(..)
    , PdfShadingData(..)
    , getShadingData
    , pdfAxialShading
    , pdfRadialShading
    ) where 

import Graphics.PDF hiding(translate)
import qualified Graphics.PDF as P
import Diagrams.Prelude
import Data.Typeable

--import qualified Debug.Trace as T 

--debug a = T.trace (show a) a

data TextOrigin = Center 
                | LeftSide 
                | RightSide 
                | TopSide 
                | BottomSide 
                | TopLeftCorner 
                | TopRightCorner 
                | BottomLeftCorner 
                | BottomRightCorner 
                deriving(Eq)

type LabelSize = Int

-- | Alpha channel for color is not taken into account in the setting
data LabelStyle = LabelStyle FontName LabelSize Justification TextOrigin (Colour Double)

data PdfTextBox = PdfTextBox { _transform :: T2 
                             , _suggestedWidth :: Double 
                             , _suggestedHeight :: Double 
                             , _computedWidth :: Double 
                             , _computedHeight :: Double
                             , _style :: LabelStyle 
                             , _text :: String
                             }

type instance V PdfTextBox = R2

instance Transformable PdfTextBox where
  transform t (PdfTextBox tt sw sh cw ch a s) = PdfTextBox (t <> tt) sw sh cw ch a s

instance IsPrim PdfTextBox

instance HasOrigin PdfTextBox where
  moveOriginTo p = translate (origin .-. p)

instance Renderable PdfTextBox NullBackend where
  render _ _ = mempty

data PdfImage = PdfImage T2 (PDFReference PDFJpeg)

type instance V PdfImage = R2

instance Transformable PdfImage where
  transform t (PdfImage tt ref) = PdfImage (t <> tt) ref

instance IsPrim PdfImage

instance HasOrigin PdfImage where
  moveOriginTo p = translate (origin .-. p)

instance Renderable PdfImage NullBackend where
  render _ _ = mempty

data PdfURL = PdfURL T2 String Double Double 

type instance V PdfURL = R2

instance Transformable PdfURL where
  transform t (PdfURL tt s w h) = PdfURL (t <> tt) s w h

instance IsPrim PdfURL

instance HasOrigin PdfURL where
  moveOriginTo p = translate (origin .-. p)

instance Renderable PdfURL NullBackend where
  render _ _ = mempty

drawStringLabel :: LabelStyle 
                -> String 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> PDFFloat 
                -> Draw () 
drawStringLabel (LabelStyle fn fs j _ fillc) s _ _ w h = do
  let pdfColor (r,g,b,_) = P.Rgb r g b
      pdffc = pdfColor . colorToSRGBA . toAlphaColour $ fillc 
  typesetText w h NormalParagraph (P.Font (PDFFont fn fs) pdffc pdffc) $ do
    setJustification j
    paragraph $ do
        txt $ s

typesetText :: (ParagraphStyle ps s, P.Style s) 
            => PDFFloat -- ^ width limit
            -> PDFFloat -- ^ height limit
            -> ps -- ^ default vertical style
            -> s -- ^ Default horizontal style
            -> TM ps s a -- ^ Typesetting monad
            -> P.Draw ()
typesetText w h ps p t = do
    let b = getBoxes ps p t
        sh = styleHeight p
        c = mkContainer 0 0 w h sh
        (d,_,_) = fillContainer (defaultVerState ps) c b
    d

getTextBoundingBox :: (ParagraphStyle ps s, P.Style s) 
                   => PDFFloat -- ^ x
                   -> PDFFloat -- ^ y
                   -> PDFFloat -- ^ width limit
                   -> PDFFloat -- ^ height limit
                   -> ps -- ^ default vertical style
                   -> s -- ^ Default horizontal style
                   -> TM ps s a -- ^ Typesetting monad
                   -> Rectangle
getTextBoundingBox _ _ w h  ps p t = 
    let b = getBoxes ps p t
        sh = styleHeight p
        c = mkContainer 0 0 w h sh
        (_,c',_) = fillContainer (defaultVerState ps) c b
    in 
    containerContentRectangle  c'


data PdfShadingData = PdfAxialShadingData P2 P2 (Colour Double) (Colour Double) 
                    | PdfRadialShadingData P2 Double P2 Double (Colour Double) (Colour Double) deriving (Show,Typeable)
newtype PdfShading = PdfShading (Last PdfShadingData) deriving (Typeable, Semigroup)

instance AttributeClass PdfShading

getShadingData :: PdfShading -> PdfShadingData
getShadingData (PdfShading (Last c)) = c

addshading :: (HasStyle a) => PdfShadingData -> a -> a
addshading s = applyAttr (PdfShading . Last $ s)


-- | Define Axial shading for a diagram
pdfAxialShading :: HasStyle a => P2 -> P2 -> Colour Double -> Colour Double -> a -> a
pdfAxialShading pa pb ca cb = addshading (PdfAxialShadingData pa pb ca cb)

-- | Define Radial shading for a diagram
pdfRadialShading :: HasStyle a 
                 => P2 -- ^ Center of inner circle
                 -> Double -- ^ Radius of inner circle
                 -> P2 -- ^ Center of outer circle
                 -> Double -- ^ Radius of outer circle
                 -> Colour Double -- ^ Inner colour
                 -> Colour Double -- ^ Outer colour
                 -> a -> a
pdfRadialShading pa ra pb rb ca cb = addshading (PdfRadialShadingData pa ra pb rb ca cb)