\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{tikz-cd}
\usepackage{graphicx}

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

\title{Free structures}
\author{Thomas Sutton}

\begin{document}
\maketitle

\begin{abstract}
We've heard quite a bit about functional programming with ``free monads'' and
``free applicatives'' (most recently from Cam and Afsal) and various extensions
(e.g. my talk on free monads with explicit fixed points). In this talk I'll try
to explain what ``free'' means and talk about some other useful (or, at least,
interesting) free constructions.
\end{abstract}

\section*{Introduction}

\begin{description}
\item[Question] How do you tell when someone is a good functional programmer?
\item[Answer] They can finish a program without getting covered in chalk dust.
\end{description}

\begin{center}
  \includegraphics{functional-programming}
\end{center}

\clearpage

\tableofcontents

\section{Introduction}


As a group it's fairly safe to say we're mostly used to ``free'' as part of
the compound word ``free-monad'' (and, if Cam has spoken recently,
``free-applicative'') but it's quite a bit more general than that. But before
we dig in, let's unpack our understanding of these familiar free things:

\begin{itemize}
\item A free monad arises when we start with a functor and add just enough
      additional structure to get a monad (i.e. return and join).
\item A free applicative arises when we start with a functor and add just
      enough additional structure to get an applicative functor (i.e. pure
      and ap).
\end{itemize}

So maybe ``free'' things are about ``adding enough extra stuff to get a
thing''? Let's see!

\section{Things}

\section{Examples}
\subsection{Semigroups}
\subsection{Monoids}
\subsection{Groups}

\section{Forgetful things}

Here describe the informal concept of forgetful functors using the stuff,
structure, properties.

\section{``Free'' things}

\[
  \begin{tikzcd}
    A \arrow{r}{f} \arrow[swap]{dr}{g\circ f} & B \arrow{d}{g} \\
     & C
  \end{tikzcd}
\]

\begin{tikzcd}
&
  A
    \arrow{ldd}[swap]{f}
    \arrow{rd}[description]{c}
    \arrow{rrd}[description]{d}
    \arrow{rrrd}[description]{e}
\\
&
  B
    \arrow{ld}
    \arrow{r}
&
  C
    \arrow{r}
&
  D
    \arrow{r}
&
  E
\\
  F
\end{tikzcd}

\subsection{Algebraic view}

\subsection{Categorical view}

\section{Programming}

\subsection{Free semigoups}
\subsection{Free monoids}
\subsection{Free groups}
\subsection{Free algebras}

\section{Conclusions}

\section*{Questions}

\section*{Bibliography}

\printbibliography

\end{document}