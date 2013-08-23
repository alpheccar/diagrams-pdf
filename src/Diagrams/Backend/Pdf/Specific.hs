{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DeriveDataTypeable        #-}
module Diagrams.Backend.Pdf.Specific(
      LabelStyle(..)
    , TextBox(..)
    , drawStringLabel
    , getTextBoundingBox
    , TextOrigin(..)
    , LabelSize
    ) where 

import Graphics.PDF hiding(translate)
import qualified Graphics.PDF as P
import Diagrams.Prelude



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
data LabelStyle = LabelStyle FontName LabelSize Justification TextOrigin SomeColor

data TextBox = TextBox { _transform :: T2 
                       , _suggestedWidth :: Double 
                       , _suggestedHeight :: Double 
                       , _computedWidth :: Double 
                       , _computedHeight :: Double
                       , _style :: LabelStyle 
                       , _text :: String
                       }

type instance V TextBox = R2

instance Transformable TextBox where
  transform t (TextBox tt sw sh cw ch a s) = TextBox (t <> tt) sw sh cw ch a s

instance IsPrim TextBox

instance HasOrigin TextBox where
  moveOriginTo p = translate (origin .-. p)

instance Renderable TextBox NullBackend where
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


   