\documentclass[a4paper]{article}
%include polycode.fmt
%if False

\begin{code}
{-# OPTIONS -XScopedTypeVariables -XGADTs -XRankNTypes -XDeriveFunctor -XDeriveTraversable -XDeriveFoldable  -XTypeOperators -fwarn-incomplete-patterns  -XNoMonomorphismRestriction -XFlexibleInstances #-}
module Main where

import Data.Typeable (Typeable)
import Data.Function (fix)
\end{code}
%endif

%format <$> = "\mathbin{<\hspace{-1.6pt}\mathclap{\raisebox{0.1pt}{\scalebox{.8}{\$}}}\hspace{-1.6pt}>}"
%format ~> = "\rightsquigarrow"
%format forall a = "\forall " a

\usepackage{lmodern}
\usepackage[utf8x]{inputenc}
\usepackage[T1]{fontenc}
\usepackage{textcomp} % provides euro and other symbols

\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{bbold}
\usepackage{color}
\usepackage{fancyvrb}
\usepackage{graphicx}
\usepackage{microtype}
\usepackage{parskip}
\usepackage{tikz-cd}
\usepackage{upquote}
\usepackage{url}

\usepackage[backend=biber,style=trad-abbrv]{biblatex}
\usepackage[hidelinks,pdftex,unicode]{hyperref}

\usepackage{mathtools}
\usepackage{ifxetex,ifluatex}
\usepackage{fixltx2e} % provides \textsubscript

\urlstyle{same}
\UseMicrotypeSet[protrusion]{basicmath}

\addbibresource{article.bib}

\usepackage{tikz}
\usetikzlibrary{calc,shapes.multipart,chains,arrows}

\title{Efficient evaluation of pure calculi}
\author{Thomas Sutton}
\date{6 March 2019}

\hypersetup{
  pdftitle={Efficient evaluation of pure calculi},
  pdfauthor={Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
In this talk I'll describe the Krivine abstract machine for executing programs
in the $\lambda$-calculus. There are a large number of more or less similar
machines that implement various semantics for different languages.
\end{abstract}

\tableofcontents

\section{Introduction}

Executing programs by direct interpretation of abstract syntax trees is
fundamentally inefficient and, even if it weren't, evaluation by substitution
is fiddley.
In this talk I'll discuss a family of abstract
machines for evaluating pure functional languages. Evaluators based on these
machines are more efficient than direct interpretation but still very simple.

%include LC.lhs

%include DB.lhs

%include Compile.lhs

%include Krivine.lhs

%if False
\begin{code}

import qualified LC
import qualified DB
import Compile
import Krivine

main :: IO ()
main = putStrLn "hello"
\end{code}
%endif

\appendix

%include LC/Fmt.lhs
%include DB/Fmt.lhs

\clearpage

\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}
