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

\addbibresource{presentation.bib}

\title{A computational sketch of homology}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {A computational sketch of homology},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
Homology is a way of assigning a sequence of algebraic objects to other mathematical
structures. In this document we'll look at two different homology theories --
cubical and simplical -- which assign sequences of groups to topological spaces.
The bulk of the document is a collection of Haskell tools for applying these
theories to representing and analysing suitable data sets.
\end{abstract}

\tableofcontents

\clearpage

\section{Introduction}

One example of this is the method of characterising manifolds (surfaces) by
counting the number and nature of the holes in them. We can differentiate
between a disk (with no holes) from a circle (with one hole) or a figure-eight
(with two holes). While a circle and a hollow sphere both have a one hole, the
circle has a 2-dimensional hole and the sphere has a 3-dimensional hole.
With enough mathematical machinery we can describe manifolds by computing a
sequence of Abelian groups which characterise (at each dimension) the holes in
the manifold.

This framework of studying some class of objects by assigning each of them a
sequence of algebraic objects which captures some interesting property is
{\it homology}.

In this document we're interested in homology theories that we can implement in
a computer and apply to analyse and understand data sets. We'll examine two
examples of such theories:

\begin{itemize}
\item {\it Simplical homology} is founded on simplices -- triangles generalised
to arbitrary dimensions.

\item {\it Cubical homology} is founded on elementary cubes -- generalised cubes
of arbitrary dimensions with vertices at integer coordinates and edges of unit
length.
\end{itemize}

Through the document we'll describe and implement a numbe of tools.

\section{A sketch of homology theory}

%include Theory.lhs

\section{Simplical homology}

%include Simplical.lhs

\section{Cubical homology}

%include Cubical.lhs

\clearpage

\appendix
\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}