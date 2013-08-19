{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Pdf.CmdLine
-- Copyright   :  (c) 2013 alpheccar.org (see LICENSE)
-- License     :  BSD-style (see LICENSE)
--
-- Convenient creation of command-line-driven executables for
-- rendering diagrams using the Pdf backend.
--
-- * 'defaultMain' creates an executable which can render a single
--   diagram at various options.
--
-----------------------------------------------------------------------------

module Diagrams.Backend.Pdf.CmdLine
       ( defaultMain
       , multipleMain
       , Pdf
       ) where

import Diagrams.Prelude hiding (width, height, interval)
import Diagrams.Backend.Pdf

import System.Console.CmdArgs.Implicit hiding (args)

import Prelude

import Data.List.Split

import System.Environment  (getProgName)
import qualified Graphics.PDF as P

data DiagramOpts = DiagramOpts
                   { width     :: Maybe Int
                   , height    :: Maybe Int
                   , output    :: FilePath
                   , compressed       :: Maybe Bool
                   , author :: Maybe String
                   }
  deriving (Show, Data, Typeable)

diagramOpts :: String -> DiagramOpts
diagramOpts prog = DiagramOpts
  { width =  def
             &= typ "INT"
             &= help "Desired width of the output image (default 400)"

  , height = def
             &= typ "INT"
             &= help "Desired height of the output image (default 400)"

  , output = def
           &= typFile
           &= help "Output file"

  , compressed = def
              &= typ "BOOL"
              &= help "Compressed PDF file"
  , author = def 
              &= typ "STRING"
              &= help "Author of the document"
  }
  &= summary "Command-line diagram generation."
  &= program prog

-- | This is the simplest way to render diagrams, and is intended to
--   be used like so:
--
-- > ... other definitions ...
-- > myDiagram = ...
-- >
-- > main = defaultMain myDiagram
--
--   Compiling a source file like the above example will result in an
--   executable which takes command-line options for setting the size,
--   output file, and so on, and renders @myDiagram@ with the
--   specified options.
--
--   Pass @--help@ to the generated executable to see all available
--   options.  Currently it looks something like
--
-- @
-- Command-line diagram generation.
--
-- Foo [OPTIONS]
-- 
-- Common flags:
--   -w --width=INT    Desired width of the output image (default 400)
--   -h --height=INT   Desired height of the output image (default 400)
--   -o --output=FILE  Output file
--   -c --compressed   Compressed PDF file
--   -? --help         Display help message
--   -V --version      Print version information
-- @
--
--   For example, a couple common scenarios include
--
-- @
-- $ ghc --make MyDiagram
--
--   # output image.eps with a width of 400pt (and auto-determined height)
-- $ ./MyDiagram --compressed -o image.pdf -w 400
-- @

defaultMain :: Diagram Pdf R2 -> IO ()
defaultMain d = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog)
  let sizeSpec = case (width opts, height opts) of
                            (Nothing, Nothing) -> Absolute
                            (Just wi, Nothing)  -> Width (fromIntegral wi)
                            (Nothing, Just he)  -> Height (fromIntegral he)
                            (Just wi, Just he)   -> Dims (fromIntegral wi)
                                                       (fromIntegral he)
      (w,h) = sizeFromSpec sizeSpec
      theAuthor = maybe "diagrams-pdf" id (author opts)
      compression = maybe False id (compressed opts)
      docRect = P.PDFRect 0 0 (floor w) (floor h)
      pdfOpts = PdfOptions sizeSpec
  ifCanRender opts $ do 
    P.runPdf (output opts) 
           (P.standardDocInfo { P.author=P.toPDFString theAuthor, P.compressed = compression}) docRect $ do
              page1 <- P.addPage Nothing
              P.drawWithPage page1 $ renderDia Pdf pdfOpts d

-- | Generate a multipage PDF document from several diagrams.
-- Each diagram is scaled to the page size
multipleMain :: [Diagram Pdf R2] -> IO ()
multipleMain d = do
  prog <- getProgName
  opts <- cmdArgs (diagramOpts prog)
  let sizeSpec = case (width opts, height opts) of
                            (Nothing, Nothing) -> Absolute
                            (Just wi, Nothing)  -> Width (fromIntegral wi)
                            (Nothing, Just he)  -> Height (fromIntegral he)
                            (Just wi, Just he)   -> Dims (fromIntegral wi)
                                                       (fromIntegral he)
      (w,h) = sizeFromSpec sizeSpec
      theAuthor = maybe "diagrams-pdf" id (author opts)
      compression = maybe False id (compressed opts)
      docRect = P.PDFRect 0 0 (floor w) (floor h)
      pdfOpts = PdfOptions sizeSpec
      createPage aDiag = do 
        page1 <- P.addPage Nothing
        P.drawWithPage page1 $ renderDia Pdf pdfOpts aDiag 
  ifCanRender opts $ do 
    P.runPdf (output opts) 
           (P.standardDocInfo { P.author=P.toPDFString theAuthor, P.compressed = compression}) docRect $ do
              mapM_ createPage d
              


ifCanRender :: DiagramOpts -> IO () -> IO ()
ifCanRender opts action =
  case splitOn "." (output opts) of
    [""] -> putStrLn "No output file given."
    ps |  last ps `elem` ["pdf"] -> action
       | otherwise -> putStrLn $ "Unknown file type: " ++ last ps

