\documentclass[a4paper]{article}
%include polycode.fmt


\usepackage{tikz-cd}
\usepackage{graphicx}
\usepackage{bbold}
\usepackage{amsmath}

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{A computational sketch of homology}
\author{Thomas Sutton}

\begin{document}
\maketitle

\begin{abstract}
Homology is a way of assigning a sequence of algebraic objects to other mathematical
structures. In this document we'll look at two different homology theories by way of
Haskell tools for applying them to analysing data sets.
\end{abstract}

\tableofcontents

\section{Introduction}

%include Cubical.lhs

%include Simplical.lhs

\section{Bibliography}

\nocite{*}

\printbibliography

\end{document}