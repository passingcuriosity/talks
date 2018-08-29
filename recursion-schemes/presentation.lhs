\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}
\usepackage{listings}
\usepackage{color}

\lstset{
  frame=none,
  xleftmargin=2pt,
  stepnumber=1,
  numbers=left,
  numbersep=5pt,
  numberstyle=\ttfamily\tiny\color[gray]{0.3},
  belowcaptionskip=\bigskipamount,
  captionpos=b,
  escapeinside={*'}{'*},
  language=haskell,
  tabsize=2,
  emphstyle={\bf},
  commentstyle=\it,
  stringstyle=\mdseries\rmfamily,
  showspaces=false,
  keywordstyle=\bfseries\rmfamily,
  columns=flexible,
  basicstyle=\small\sffamily,
  showstringspaces=false,
  morecomment=[l]\%,
}

\title{Zygohistomorphic prepromorphisms: An introduction to recursion schemes}
\author{Thomas Sutton}

\begin{document}
\maketitle

\begin{abstract}
Recursions schemes are useful ways of thinking implementing recursive operations, but they can also lead to
``ha ha only serious'' jokes like {\it zygohistomorphic prepromorphisms}. In this talk I'll try to explain
what this unwieldy name means and a few related (and, hopefully, more useful) recursion schemes.
\end{abstract}

\tableofcontents

\section{Recursion combinators}

\section{Categorising recursion schemes}

We can see that there are some broad categories that recursive programs fall into:

\begin{itemize}
\item We can tear a structure down.
\item We can build a structure up.
\item We can transform a structure.
\item We can do some combination of the above.
\end{itemize}

\section{Qualifying recursion schemes}

\section{Implementing recursion schemes}

\begin{itemize}
\item Like algebraic structures: Monoid, Semigroup, Group, etc.

\item Like categorical structures: Functor, natural transformation, Monad,
  Comonad, etc.

\item Indeed, like pretty much anything that less powerful languages describe
  by analogy in prose documentation.
\end{itemize}

We can build on (some of) these to directly implement recursion schemes in
sufficiently powerful programming languages.

\section{Bestiary}

\subsection{Folds}


\subsubsection{Cata-}

tears down a structure level by level



\subsubsection{Para-}

tears down a structure with primitive recursion



\subsubsection{Zygo-}

tears down a structure with the aid of a helper function



\subsubsection{Histo-}

tears down a structure with the aid of the previous answers it has given.



\subsubsection{Prepro-}

tears down a structure after repeatedly applying a natural transformation


\subsection{Unfolds}


\subsubsection{Ana-}

builds up a structure level by level



\subsubsection{Apo-}

builds up a structure opting to return a single level or an entire branch at each point



\subsubsection{Futu-}

builds up a structure multiple levels at a time



\subsubsection{Postpo-}

builds up a structure and repeatedly transforms it with a natural transformation


\subsection{Refolds}


\subsubsection{Hylo-}

builds up and tears down a virtual structure



\subsubsection{Chrono-}

builds up a virtual structure with a futumorphism and tears it down
with a histomorphism



\subsubsection{Synchro-}

a high level transformation between data structures using a third data structure to queue intermediate results



\subsubsection{Exo-}

a high level transformation between data structures from a trialgebra to a bialgebraga



\subsubsection{Meta-}

a hylomorphism expressed in terms of bialgebras

or:

A fold followed by an unfold; change of representation



\subsubsection{Dyna-}

builds up a virtual structure with an anamorphism and tears it down with a histomorphism


\section{Zygohistomorphic prepromorphisms}

\begin{enumerate}
\item zygo- tears down with the aid of a helper function.

\item histo- tears down with the aid of the previous answers it has given.

\item repro- tears down after repeatedly applying a natural transformation.
\end{enumerate}

\begin{code}
zygoHistoPrepro
  :: (Corecursive t, Recursive t)
  => (Base t b -> b)
  -> (forall c. Base t c -> Base t c)
  -> (Base t (EnvT b (Cofree (Base t)) a) -> a)
  -> t
  -> a
zygoHistoPrepro f g t = gprepro (distZygoT f distHisto) g t
\end{code}

\section*{Questions}

{\center{???}}

\nocite{*}
\printbibliography[title=Bibliography]

\end{document}
