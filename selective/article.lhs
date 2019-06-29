\documentclass[12ptfont,a4paper]{article}
%include polycode.fmt

%if False
\begin{code}
{-# LANGUAGE LambdaCase, FlexibleInstances #-}
\end{code}
%endif
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

\addbibresource{article.bib}


\title{Selective Functors}
\author{Thomas Sutton}
\date{15 May 2019}

\hypersetup{
  pdftitle = {Selective Functors},
  pdfauthor = {Thomas Sutton}
}

\newcommand{\typeclass}[1]{#1}

\begin{document}
\maketitle
\tableofcontents

\pagebreak

\begin{abstract}
We can use a number of different interfaces when writing effectful programmes
depending on the degree of freedom (and commensurate lack of language-assisted
safety) we want.

\begin{itemize}

\item
Programming against \typeclass{Functor}s allows us to describe standalone
computations.

\item
Programming against \typeclass{Applicative} functors allows us to describe
``static'' graphs of computations (where we can combined several effectful
computations, but they must be known statically at compile time).

\item
Programming against \typeclass{Monad} allows us to describe ``dynamic'' graphs
of computations (where runtime results can result in a different ``shape'' of
computation).
\end{itemize}

\typeclass{Selective} functors slot into this hierarchy of expressive power between 
Applicative Functors and Monads: they require the programmer to declare the
shape of an effectful computation statically (like Applicative Functors) but
allow the effects to be executed (or not) dynamically (like Monad).

\end{abstract}

\section{Introduction}

\section{\typeclass{Functor} - atomic effects}

\begin{code}
instance Functor f where
  fmap :: (a -> b) -> f a -> f b
\end{code}

\section{\typeclass{Applicative} - multiple static effects}

\begin{code}
instance (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
\end{code}

\section{\typeclass{Monad} - multiple dynamic effects}

\begin{code}
class (Applicative f) => Monad f where
  (>>=) :: m a -> (a -> m b) -> m b
\end{code}

\section{\typeclass{Selective} - multiple static selected effects}

\begin{code}
instance (Applicative f) => Selective f where
  select :: f (Either a b) -> f (a -> b) -> f b
\end{code}

\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}