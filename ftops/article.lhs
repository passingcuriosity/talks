{-# LANGUAGE LambdaCase, FlexibleInstances #-}
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

\addbibresource{article.bib}

\defbibenvironment{midbib}
  {\list
     {}
     {\setlength{\leftmargin}{\bibhang}%
      \setlength{\itemindent}{-\leftmargin}%
      \setlength{\itemsep}{\bibitemsep}%
      \setlength{\parsep}{\bibparsep}}}
  {\endlist}
  {\item}

\newcommand{\underlying}[1]{\lvert #1 \rvert}

\title{Final tagless operational semantics}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {Final tagless operational semantics},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
Final tagless is dead. Long live final tagless.
\end{abstract}

\section*{Introduction}

\begin{quote}
कालो ऽस्मि लोकक्षयकृत् प्रवृद्धो \\
लोकान्समाहर्तुमिह प्रवृत्तः . \\
ऋते ऽपि त्वां न भविष्यन्ति सर्वे \\ 
ये ऽवस्थिताः प्रत्यनीकेषु योधाः

kālo 'smi lokakṣayakṛt pravṛddho \\
lokān samāhartum iha pravṛttaḥ \\
ṛtepi tvāṃ na bhaviṣyanti sarve \\
yevasthitāḥ pratyanīkeṣu yodhāḥ

-- Bhagavad Gita 11.32
\end{quote}

\section{A language}

The language here is a simple call by value untyped lambda-calculus with De Bruijn-indexed
variables -- rather than using {\it names} for variables we use {\it indexes} into some sort
of linear data-structure (locations in the stack frame, values in a vector, etc.)

We can represent this language with an interface which has standard abstraction 

\begin{code}
class Lambda repr where
  lam :: repr -> repr               -- \....
  app :: repr -> repr -> repr       -- (...) (...)

  vz :: repr                        -- x_0
  vs :: repr -> repr                -- x_(n+1)

  int :: Int -> repr                -- 12
  inc :: repr                       -- "... + 1"
  ifz :: repr                       -- ifz ... then ... else ...
\end{code}

\section{An implementation}

We can implement this interface with a $repr$ that takes a context (the data structure
containing the values of any free variables) to a value (either an integer or a
partially applied function).

\begin{code}
data V = I Int | A (V -> V)

instance Lambda ([V] -> V) where
  int x  =  \ _   ->   I x
  inc    =  \ _   ->   A (\ (I x) -> I (x+ 1))
  ifz    =  \ env ->   A (\ x -> A (\t -> A (\f -> 
      case (x, t, f) of
        (I 0, A k, _)          -> k x
        (I n, _, A k) | n > 0  -> k (I $ n - 1)
    )))

  vz    =  \ (h:_) -> h
  vs e  =  \ (_:r) -> e r

  lam e      = \ env -> A (\x -> e (x:env))
  app e1 e2  = \ env -> case e1 env of A f -> f (e2 env)
\end{code}

Hopefully you can kind of follow what's going on here. Every "value" is a
computation that takes some inputs and produces a result (either an int or a
partially applied function). This being an untyped language we'll just have to
hope that evaluating a program yields a result that doesn't need any more input
and produces an int.

\graphicx{hope}



\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}