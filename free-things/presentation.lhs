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

\UseMicrotypeSet[protrusion]{basicmath} % disable protrusion for tt fonts
\urlstyle{same}  % don't use monospace font for urls

\usepackage[backend=biber,style=trad-abbrv,firstinits=true,citestyle=authoryear]{biblatex}
\addbibresource{presentation.bib}

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

\title{Free structures}
\author{Thomas Sutton}

\hypersetup{
  pdftitle = {Free structures},
  pdfauthor = {Thomas Sutton}
}

\begin{document}
\maketitle

\begin{abstract}
We've heard quite a bit about functional programming with ``free monads'' and
``free applicatives'' (most recently from Cam and Afsal) and various extensions
(e.g. my talk on free monads with explicit fixed points). In this talk I'll try
to explain what ``free'' means and talk about some other useful, different, or,
at least, different free constructions.
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

First, let's talk about ``things''. We often just ignore the nature of the
various things we discuss. When we're talking about category theory in general,
it is not true that objects are like sets or types and that arrows are like
functions. There are lots and lots of categories where this is not even remotely
true.

$\mathbf{Mat}_{\mathbb{k}}$ is a category where the objects are natural numbers
and arrows are matrices on the field $\mathbb k$ with dimensions from the target
and source.

\begin{center}
\begin{tikzcd}
2
  \arrow{rr}{f: 3\times 2}
  \arrow{ddrr}{g \cdot f : 4\times 2}
&
&
3
\arrow{dd}{g : 4 \times 3}
\\
\\
&
&
4
\\
\end{tikzcd}
\end{center}

\begin{align*}
Hom(m,n)   \subset \mathbb{k}^{n \times m} \\
g \circ f  :  Hom(n, o) \times Hom(m, n) \rightarrow Hom(m, o) &= & g f \\
Id_{n}     \in Hom(n,n) & = & (\delta^{i}_{j})^{n\times n} \\
\end{align*}

$f : m \rightarrow n$ are $n \times m$ matrixes, $id_{n} : n \rightarrow n$ is
the $n \times n$ identity matrix, and composition is matrix multiplication.

But we aren't talking about these categories. As usual we're interested in
categories (1) of structures that we want to manipulate or (2) of handwaving
about programs: categories like $\mathbf{Mon}$, $\mathbf{Grp}$, and
``$\mathbf{Hask}$''. You can assume that every category in sight is concrete,
by the whichever forgetful functor seems obvious or by handwaving.

\section{Things}
\subsection{Semigroups}

A semigroup is a set with a binary operation on that set and some laws
requiring that the operation is associative.

$(S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

\subsection{Monoids}

A monoid is a semigroup with an extra distinguished element and an additional
law asserting that this element is a zero for the operator.

$(S, \epsilon \in S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

\subsection{Groups}

A group is a monoid with an inverse for every element. This acts as an inverse
should: it ``reverses'' the action of the element.

$(S, \epsilon \in S, \:^{-1} : S \rightarrow S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

$\forall x \in S.\: x \cdot x^{-1} \equiv \epsilon \equiv x^{-1} \cdot x$

\subsection{Abelian groups}

An Abelian group is a group with an additional law asserting that the
operator is commutative:

$(S, \epsilon \in S, \:^{-1} : S \rightarrow S, \cdot : S \times S \rightarrow S)$

$\forall x,y,z \in S.\: (x\cdot y)\cdot z \equiv x\cdot (y\cdot z)$

$\forall x \in S.\: x \cdot \epsilon \equiv x \equiv \epsilon \cdot x$

$\forall x \in S.\: x \cdot x^{-1} \equiv \epsilon \equiv x^{-1} \cdot x$

$\forall x, y \in S.\: x \cdot y \equiv y \cdot x$

\subsection{Categories}

A category $\mathcal{C}$ is composed of the following data:

$Ob(\mathcal{C})$ is a collection of {\it objects}.

$Hom_{\mathcal{C}}(A,B)$ is a collection of {\it morphisms} between two objects.

$Id : Ob(\mathcal{C}) \rightarrow Hom(A,A)$.

$\circ : Hom(B,C) \times Hom(A,B) \rightarrow Hom(A,C)$ composes two morphisms
which meet at a common object.

$\forall A,B \in Ob(\mathcal{C}).\: \forall f \in Hom(A,B).\: f \circ Id_A \equiv f \equiv Id_B \circ f$

\section{Things for free}

So all those things (and many, many, more) exist but we often don't want to
bother hunting down the correct one (supposing there is even a correct {\it one}
for our purposes).
We might want to do {\it monoid}-ish or {\it group}-ly things to some stuff we
just have lying around and figure out what it all means later. In these
situations we want a structure for {\it free}.

The basic idea is that we can construct these and other (but not all) structures
freely from some simple inputs and in such such a way that we can always convert
the answer into some other choice of particular monoid, group, etc.

We'll also see that being a ``free {\it foo}'' is not a definition. It's some
structure (in the categorical sense) that may or may not be associated with an
object.

\subsection{Free monoids}

In an introductor algebra text we're likely to see a definition of a monoid
{\it freely generated} from a set A like this:

\begin{itemize}
\item Every element $m \in M$ can be written as a product of elements
  of $A$:
$$m = a_1 \cdot_M a_2 \cdot_M ... \cdot_M a_n, a_i \in A$$
\item Only relations required by the monoid laws hold in $M$. (i.e. if $m = n$,
then it's because of the monoid laws and not something funky from $A$).
\end{itemize}

What do these requirements give us? Let's draw a diagram! Every monoid has an
underlying set of elements: $\underlying{M}$.

\begin{center}
\begin{tikzcd}
  M(A)
  \arrow[dotted]{rr}{\overline{f}}
&& N
\\
\\
  \underlying{M(A)}
  \arrow{rr}{\underlying{\overline{f}}}
&& \underlying{N}
\\\\
  A
  \arrow{uu}{i}
  \arrow{uurr}{f}
&
\end{tikzcd}
\end{center}

If we start with set $A$ and an injection function $i$ into $\underlying{M(A)}$
then for any other monoid $N$ and function $f : A \rightarrow \underlying{N}$ we
can construct a unique monoid homomorphism $\overline{f} : M(A) \rightarrow N$
such that $\underlying{\overline{f}} \circ i \equiv f$.

Which is a long-winded way of say that: $M(A)$ is a free monoid on $A$ if,
whenever we have some other monoid $N$ and some other way of turning values of
$A$ into values of $N$, then we can magic up a way to convert things in $M(A)$
into things in $N$ that lines up with the other things.

Note that this doesn't say that $M(A)$ is {\it unique}!

\section{Implementing free things}

There are a few ways that we can think of constructing a free structures:

\begin{itemize}
\item Formally - Build up strings of symbols (from $A$ and the monoid operations)
and use the laws to work out which strings are equivalent. The elements of $M(A)$
are then the equivalence classes of strings.
\item Structurally - Find some sort of structure with appropriate operations
which obey the require laws. (Note that this is going to be functorial.)
\end{itemize}

The first is very easy to describe and more than a little annoying to use
(implement), while the second might be tricky to find but much easier to put
to work.

\subsection{Free monoids}

Remember, we have $A$ and $i : A \rightarrow \underlying{M(A)}$. We'll take $A$
to be a type, then $M$ is going to be a type constructor. We need a way for $i$
to put things into $|M(A)|$

\begin{code}
data Mon a
  = Elem a
  | Empty
  | Cat (Mon a) (Mon a)

instance Monoid (Mon a) where
  mempty = Empty
  mappend Empty n = n
  mappend m Empty = m
  mappend m n = Cat m n
\end{code}

This has an empty element which forms an identity but isn't associative. We've
build a tree which {\it encodes} the association rather than {\it ignores} it.

So instead of a tree-ish constructor, maybe we could build a sequential one?

\begin{code}
data Mon a
  = Empty
  | Cat a (Mon a)

instance Monoid (Mon a) where
  mempty = Empty
  mappend  Empty      n      = n
  mappend  m          Empty  = m
  mappend  (Cat a m)  n      = Cat a (mappend m n)
\end{code}

Hmm. That looks familiar.

\begin{code}
data [a]
  = []
  | a : [a]

instance Monoid [a] where
  mempty = []
  mappend  []       n   = n
  mappend  m        []  = m
  mappend  (a : m)  n   = a : (mappend m n)
\end{code}

So how do we magic up the conversion function given @f :: Monoid n => a -> n@?
We're need to apply @f@ to the @A@ elements inside the free monoid and then monoid
them all together. We need to fold @f@ and @mappend@:

\begin{code}
conv :: (Monoid n) => (a -> n) -> [a] -> n
conv f m = foldr (\a n -> f a `mappend` n) mempty m
\end{code}

So we can have something like this:

\begin{code}
type Monoid a = [a]
type A = Int

inj :: A -> Monoid A
inj a = [a]

f :: A -> Max Double
f a = Max (fromIntegral a)

h :: Monoid Int -> Max Double
h = conv f
\end{code}


\subsection{Free semigoups}

It shouldn't take much to convince ourselves that a similar construction of free
semigroups is the non-empty list. We no longer have an @Empty@ case but otherwise
things are pretty similar.

\subsection{Vector spaces}

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

\subsection{Free categories}

If we have a directed graph $G = (V, E \subset E\times E)$ we can define a free
category by taking the vertices as objects and adding identify morphisms together
with one morphisms in $Hom(A,B)$ for each distinct path from $A$ to $B$ in the
graph. Then it's relatively straightforward to implement a composition operation
for those morphisms.

For full details see \cite{awodey}.

One way to actually do this (mirroring the above) is to take a morphism in this
category as a list of edges:

\begin{code}
type Edge = (Node, Label, Node)
type Morphism = NonEmptyList Edge
\end{code}

\subsection{Free monads}

A free monad we need to start with an endo-functor $F$ and magic up:

\begin{itemize}
\item An endofunctor $M : \mathcal{C} \rightarrow \mathcal{C}$
\item A natural transformation $\eta : Id \Rightarrow M$
\item A natural transformation $\mu : M \circ M \Rightarrow M$
\end{itemize}

These are @return :: Monad => a -> m a@ and @join :: Monad m => m (m a) -> m a@
familiar from Haskell (and @pure@ and @flatten@ from Scala) in a slightly
different guise.

Instead of starting with a set (type) and building a new set (type) equiped
with some gadgets, we're starting with an endofunctor and building a new
endofunctor equiped with some gadgets! And, further more, (in the right
category: the category of endofunctors and natural transformations) this is
just a monoid.

\begin{code}
data Free f a = Return a | Roll (f (Free f a))

eta :: Functor f => f a -> Free f a
eta fa = Roll $ fmap Return fa

mu :: Functor f => Free f (Free f a) -> Free f a
mu (Return fa)  = fa
mu (Roll ffa)   = Roll (fmap mu ffa)
\end{code}

\section{Observations}

What goes above is mostly baby level algebra and category theory. The
characterisation of ``free'' here only applies in concrete categories. The more
general approach (free functors are left-adjoint to forgetful functors) applies
more widely.

\section{Bibliography}

\nocite{*}

\printbibliography[heading=none]

\end{document}