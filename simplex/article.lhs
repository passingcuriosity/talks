%if False
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
module Simplex where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Equality

\end{code}
%endif
\documentclass[a4paper]{article}
%include polycode.fmt
\usepackage{amsmath}
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

\urlstyle{same}
\UseMicrotypeSet[protrusion]{basicmath}

\usepackage{amsthm}
\theoremstyle{definition}
\newtheorem{example}{Example}[section]

\newcommand{\algorithm}[1]{#1}

\addbibresource{article.bib}

\title{Linear programming and the simplex method}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {Linear programming and the simplex method},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
    This document contains notes on linear programming and on using the simplex
    method to solve linear programming problems. It includes na\"{i}ve Haskell
    code to represent linear programming problems and solve them with the
    simplex method.
\end{abstract}

\tableofcontents

\clearpage

\section{Introduction}

Linear programming is a field of optimisation which addresses problems expressed
in terms of systems of linear inequalities. This class of problems has several
interesting properties:

\begin{itemize}
\item it includes a great many problems of practical interest; and
\item we have very good techniques to solve them.
\end{itemize}

\section{Linear programming}

\section{\algorithm{Simplex} method}

The \algorithm{simplex method} is a decidable algorithm for finding the optimal
solution (if one exists) to a linear programming problem. The basic algorithm
goes like this:

\begin{enumerate}
\item Normalise the problem into a canonical form
\item Build a tableau describing the canonicalised problem
\item Use row operations to eliminate
\end{enumerate}

\section{Implementation}

%include DSL.lhs

\begin{code}
problem = do
  a <- var "a"
  b <- var "b"
  c <- var "c"

  a + b + c `leq` 25
  2 * a + 2 * b + 2 * c `leq` 50
  c `leq` 6
  a `bounded` (1,20)

  minimise $ a

data Objective n = Objective { direction :: Dir, fn :: Linear n }
data Constraint n = Constraint { fn :: Linear n, rel :: Rel, val :: Value}

newtype Constraints n = NonEmptyList (Constraint n)

data Problem n = Problem { objective :: Objective n, constraints :: Constraints n }

maximise :: Nat n => Objective n -> Constraints n -> Problem n
minimise :: Nat n => Objective n -> Constraints n -> Problem n

data family Linear n

data instance Linear 0 = forall v. Expr v
data instance Linear (Succ n) = Var -> Linear n

maximise (\a b c d -> 12 * a + 14 )


prob = do 
  maximise $ \a b c d -> 2 * a + 3 * b - 12 * c - d
  constrain $ \a b c d -> 12 * a `leq` 90
  constrain $ \a b c d -> a + b + c + d `leq` 100
\end{code}

\appendix

\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}
