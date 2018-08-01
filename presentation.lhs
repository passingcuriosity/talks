\documentclass[a4paper]{article}
%include polycode.fmt

\usepackage{tikz-cd}
\usepackage{graphicx}
\usepackage{bbold}


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

A semigroup is a set with a binary operation on that set and some laws
requiring that the operation is associative.

$(S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

\subsection{Monoids}

A monoid is a semigroup with an extra distinguished element and an additional
law asserting that this element is a zero for the operator.

$(S, \epsilon, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

\subsection{Groups}

A group is a monoid with an inverse for every element. We generally
encode that as an ``inverse'' operation.

$(S, \epsilon, \:^{-1} : S \rightarrow S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

$\forall x \in S.\: x \cdot x^{-1} == \epsilon == x^{-1} \cdot x$

\subsection{Abelian groups}

An Abelian group is a group with an additional law asserting that the
operator is commutative:

$(S, \epsilon, \:^{-1} : S \rightarrow S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

$\forall x \in S.\: x \cdot x^{-1} \equiv \epsilon \equiv x^{-1} \cdot x$

$\forall x, y \in S.\: x \cdot y \equiv y \cdot x$

\section{Free and forgetful functors}

\section{``Forgetful'' things}

Here describe the informal concept of forgetful functors using the stuff,
structure, properties.

\section{``Free'' things}

Given a set, we can construct a vector space by taking (for some ground
field $\mathbb k$) finite formal sums of its elements.
We can likewise convert any function $f : S \rightarrow T$ between sets
into a linear function between vector spaces.

${\mathbb k}[-] : {\bf Set} \rightarrow {\bf Vect}_{\mathbb k}$

We can also define a forgetful functor which takes a vector space to a
basis set.

$U : {\bf Vect}_{\mathbb k} \rightarrow {\bf Set}$

When given a vector space we can recover a set by finding a basis for it (see
your favourite linear algebra text book).

We can see how these fit together with morphisms in ${\bf Set}$ and 
${\bf Vect}_{\mathbb k}$ in the following commuting diagram:

\begin{center}
\begin{tikzcd}
S
  \arrow{rr}{{\mathbb k}[-]}
  \arrow{dd}{f}
&
&
{\mathbb k}[S]
\arrow{dd}{g}
\\
\\
U(V)
&
&
V \arrow{ll}{U}
\\
\end{tikzcd}
\end{center}

\subsection{Categorical view}

\subsection{Free semigoups}

For a given set $A$, we can call the free semigroup $A^{+}$.

This should be familiar from the $+$ iteration operator from regular languages.

\subsection{Free monoids}

We might like to call the free monoid on the set $A$, $A^{*}$.

This should be familiar from the $*$ iteration operator from regular languages.

\subsection{Free groups}

\subsection{Free algebras}

\section{Conclusions}

\section*{Questions}

\appendix{Diagrams}

\section*{Bibliography}

\printbibliography

\end{document}