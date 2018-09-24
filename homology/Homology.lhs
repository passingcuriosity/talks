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
            pdftitle={A computational sketch of homology},
            pdfauthor={Thomas Sutton},
            pdfborder={0 0 0},
            breaklinks=true}
\urlstyle{same}
\usepackage{color}
\usepackage{fancyvrb}

\usepackage[backend=biber,style=trad-abbrv]{biblatex}
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

\clearpage

\section{Introduction}

Homology is a way of assigning a sequence of algebraic objects to other mathematical
structures. In this document we're interested in homology theories that can help us
analyse and understand topological spaces using techniques which can be implemented
in computer programs. We'll examine two examples of such theories:

\begin{itemize}
\item {\it Simplical homology} is founded on simplices -- triangles generalised
to arbitrary dimensions.
\item {\it Cubical homology} is founded on elementary cubes -- cubes of
arbitrary dimensions restricted to vertices at integer coordinates and edges
of unit length.
\end{itemize}

\section{A quick sketch}

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