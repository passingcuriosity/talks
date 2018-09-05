%if False
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module PartialOrder where

import Data.Maybe
import Data.Ord

import qualified Prelude as P
\end{code}
%endif
\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{amsmath}
\usepackage{bbold}
\usepackage{graphicx}
\usepackage{tikz-cd}
\usepackage{url}
\usepackage{upquote}
\usepackage{microtype}
\usepackage{parskip}
\usepackage[pdftex,unicode]{hyperref}
\usepackage{color}
\usepackage{fancyvrb}

\UseMicrotypeSet[protrusion]{basicmath}
\urlstyle{same}

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{Orders}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {Orders},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
Let's look at partial orders, lattices, covers, etc.
\end{abstract}

\section{Introduction}

\section{Comparisons}

\begin{spec}
data Ordering = LT | EQ | GT deriving (Eq, Show)
\end{spec}

\begin{code}
class POrder t where
  compare :: t -> t -> Maybe Ordering

instance Ord t => POrder t where
  compare i j = Just $ P.compare i j

newtype Divisible i = Divisible { fromDivisible :: i }

instance Integral i => POrder (Divisible i) where
  compare (Divisible i) (Divisible j)
    | i == j            = Just EQ
    | (j `rem` i) == 0  = Just LT
    | (i `rem` j) == 0  = Just GT
    | otherwise         = Nothing
\end{code}

\end{document}