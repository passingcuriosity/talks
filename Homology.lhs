\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{url}
\usepackage{tikz-cd}
\usepackage{graphicx}
\usepackage{bbold}
\usepackage{amsmath}
\usepackage{upquote}
\usepackage[]{microtype}
\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts
\usepackage{parskip}
\usepackage{hyperref}
\hypersetup{unicode=true,
            pdftitle={So you think you can map},
            pdfauthor={Thomas Sutton},
            pdfborder={0 0 0},
            breaklinks=true}
\urlstyle{same}  % don't use monospace font for urls
\usepackage{color}
\usepackage{fancyvrb}

\usepackage[backend=biber,style=trad-abbrv,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{A computational sketch of homology}
\author{Thomas Sutton}

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

\section{Introduction}

Homology is a way of assigning a sequence of algebraic objects to other mathematical
structures. 

\section{Homology}

\subsection{Spaces and cycles}

\subsection{Chain complex}

\subsection{Algebra}

\subsection{Functors}

The various types of homology theory (including the two we'll see below) each
arises from a functor into the category of chain complexes. Once the objects are
mapped to chain complexes, the same mechanism (another functor) takes them to
homology groups in the same way.

%include Simplical.lhs

%include Cubical.lhs

\section{Bibliography}

\nocite{*}

\printbibliography

\end{document}