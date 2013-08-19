
_diagrams-pdf_ is a an [HPDF] backend for [diagrams]. Diagrams is a powerful,
flexible, declarative domain-specific language for creating vector graphics,
using the [Haskell programming language][haskell].

[diagrams]: http://projects.haskell.org/diagrams/
[haskell]: http://www.haskell.org/haskellwiki/Haskell
[HPDF]: http://hackage.haskell.org/packages/archive/HPDF/1.4.5/doc/html/Graphics-PDF-Documentation.html

_diagrams-pdf_ is a work in progress, and some features are not implemented yet. However, it 
is functional enough. 

# Installation

```
cabal update && cabal install diagrams-pdf
```

# Usage

A simple example that uses _diagrams-pdf_ to draw a square.

``` haskell
import Diagrams.Prelude
import Diagrams.Backend.Pdf.CmdLine

b1 = square 20 # lw 0.002

main = defaultMain (pad 1.1 b1)
```

Save this to file named `Square.hs` and compile this program:

```
ghc --make Square.hs
```

This will generate an executable which, when run produces a Pdf file. Run the
executable with the `--help` option to find out more about how to call it.

```
$ ./Square --help
Command-line diagram generation.

Square [OPTIONS]

Common flags:
  -w --width=INT    Desired width of the output image (default 400)
  -h --height=INT   Desired height of the output image (default 400)
  -o --output=FILE  Output file
  -c --compressed   Compressed PDF file
  -? --help         Display help message
  -V --version      Print version information
```

You _must_ pass an output file name with a `.pdf` extension to generate the PDF
file.

```
$ ./Square -o square.pdf
```

# Limitations

Two goals of the HPDF library were : powerful typesetting and no dependence on the OS libraries (for better portability).
But, HPDF was also my first big library mainly done to learn. So I had to limit myself.

As a consequence, HPDF is only using the standard fonts defined in the PDF specification. It is not (yet) possible
to include and use any other fonts.

So, for diagrams it is causing a problem : the face setting has nearly no meaning.
Face is currently selected from the Italic and Oblique settings. Times is the default but when
oblique is chosen, Helvetica is used.

I hope to improve this in future releases.

Here is a list of the fonts in HPDF:

* Helvetica	 
* Helvetica_Bold	 
* Helvetica_Oblique	 
* Helvetica_BoldOblique	 
* Times_Roman	 
* Times_Bold	 
* Times_Italic	 
* Times_BoldItalic	 
* Courier	 
* Courier_Bold	 
* Courier_Oblique	 
* Courier_BoldOblique	 
* Symbol	 
* ZapfDingbats
