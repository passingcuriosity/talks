\documentclass[]{article}
%include polycode.fmt

%if False
\begin{code}
{-# OPTIONS -XScopedTypeVariables -XGADTs -XRankNTypes -XDeriveFunctor -XDeriveTraversable -XDeriveFoldable  -XTypeOperators -fwarn-incomplete-patterns  -XNoMonomorphismRestriction -XFlexibleInstances #-}

import Data.Typeable (Typeable)
import Data.Function (fix)
\end{code}
%endif
\usepackage{comment}

\usepackage{mathtools}
\usepackage{lmodern}
\usepackage{amssymb,amsmath}
\usepackage{ifxetex,ifluatex}
\usepackage{fixltx2e} % provides \textsubscript
\usepackage[T1]{fontenc}
\usepackage[utf8]{inputenc}
\usepackage{textcomp} % provides euro and other symbols

%format <$> = "\mathbin{<\hspace{-1.6pt}\mathclap{\raisebox{0.1pt}{\scalebox{.8}{\$}}}\hspace{-1.6pt}>}"
%format ~> = "\rightsquigarrow"
%format forall a = "\forall " a

% use upquote if available, for straight quotes in verbatim environments
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
\usepackage[backend=biber,
style=alphabetic,
citestyle=authoryear
]{biblatex}
\addbibresource{presentation.bib}


\usepackage{tikz}
\usetikzlibrary{calc,shapes.multipart,chains,arrows}

\makeatletter

\tikzset{
  dotloop above/.style={
    *->,
    looseness=#1,
    to path={
      \pgfextra{%
        \let\myTikZtostart\tikztostart
        \let\myTikZ@@tonodes\tikz@@tonodes
      }
      (\myTikZtostart.two ||- \myTikZtostart.east) -- (\myTikZtostart.east) 
      to[out=0,in=0]  ([shift=(up:+.3cm)] \myTikZtostart.north east) -- ([shift=(up:+.3cm)] \myTikZtostart.north west)
      {\pgfextra{\tikz@@node@@is@@a@@labeltrue}\myTikZ@@tonodes} to[out=180, in=180] (\myTikZtostart.west)
    }
  },
  dotloop above/.default=1.5,
  dotloop left/.style={
    *->,
    looseness=#1,
    to path={
      \pgfextra{%
        \let\myTikZtostart\tikztostart
        \let\myTikZ@@tonodes\tikz@@tonodes
      }
      (\myTikZtostart.one ||- \myTikZtostart.west) -- (\myTikZtostart.west) 
      to[out=180,in=180]  ([shift=(up:+.3cm)] \myTikZtostart.north west)
      {\pgfextra{\tikz@@node@@is@@a@@labeltrue}\myTikZ@@tonodes}
      to[out=0, in=120] ([shift=(west:+.01cm)] \myTikZtostart.north)
    }
  },
  dotloop left/.default=1.5,
  dotloop right/.style={
    *->,
    looseness=#1,
    to path={
      \pgfextra{%
        \let\myTikZtostart\tikztostart
        \let\myTikZ@@tonodes\tikz@@tonodes
      }
      (\myTikZtostart.three ||- \myTikZtostart.east) -- (\myTikZtostart.east)
      to[out=0,in=0]  ([shift=(up:+.3cm)] \myTikZtostart.north east)
      {\pgfextra{\tikz@@node@@is@@a@@labeltrue}\myTikZ@@tonodes}
      to[out=180, in=60] ([shift=(east:+.01cm)]\myTikZtostart.north)
    }
  },
  dotloop right/.default=1.5
}
\makeatother


\title{So you think you can map {[}safely{]}}
\providecommand{\subtitle}[1]{}
\subtitle{Or: a Fix for Free}
\author{Thomas Sutton}
\date{25 July 2018}

\begin{document}
\maketitle

\hypertarget{introduction}{%
\section{Introduction}\label{introduction}}

\begin{itemize}
\item
  Algebraic data types\ldots{}
\item
  \ldots{}for trees.
\item
  \ldots{}for graphs.
\item
  Problems.
\item
  Solutions.
\end{itemize}

\hypertarget{algebraic-data-types}{%
\section{Algebraic Data Types}\label{algebraic-data-types}}

Most of us are familiar with algebraic data types: data types composed
of sums of products, probably with labels on the various bits and pieces
so we can easily tell them apart.

I'll use Haskell syntax because it's objectively better (and the GADT
syntax too):

\begin{code}
data Operation a where
  Dab     ::                          Operation a
  Krump   ::                 Int  ->  Operation a
  Shuffle ::                 [a]  ->  Operation a
  Robot   :: Typeable a =>   a    ->  Operation a
\end{code}

\hypertarget{recursive-types-give-trees}{%
\subsection{\texorpdfstring{Recursive \emph{types} give
trees}{Recursive types give trees}}\label{recursive-types-give-trees}}

We're also familiar with recursive types to define structures which can
contain other instances of themselves: lists, trees, etc.

\begin{spec}
data List a where
  Nil   ::                  List a
  Cons  :: a -> List a ->   List a
\end{spec}

These define trees. ``\emph{Inside}'' a @Cons@ value is an
@a@ and another (``smaller'') @List a@ value.

\hypertarget{recursive-values-give-graphs}{%
\subsection{\texorpdfstring{Recursive \emph{values} give
graphs}{Recursive values give graphs}}\label{recursive-values-give-graphs}}

But many languages have a constructs which allow us to construct
recursive values: values which are defined \emph{in terms of
themselves}. This shouldn't be surprising (indeed, recursive type
definitions work the same way).

\begin{itemize}
\item
  @letrec@ in Scheme, Racket, etc.
\item
  @let rec@ in Ocaml, @val rec@ in SML.
\item
  @fix@ in various languages.
\item
  pretty much every binding form in Haskell.
\end{itemize}

\hypertarget{infinite-lists}{%
\subsection{``Infinite'' lists}\label{infinite-lists}}

\begin{spec}
one   :: List Int
one   = Cons 1 Nil

ones  :: List Int
ones  = Cons 1 ones
\end{spec}

\begin{tikzpicture}[list/.style={rectangle split, rectangle split parts=2,
    draw, rectangle split horizontal}, >=stealth, start chain]
  \node[list, on chain] (A) {1};
  \path (A) edge[dotloop above] node {} (A);
\end{tikzpicture}

\hypertarget{infinite-binary-trees}{%
\subsection{``Infinite'' binary trees}\label{infinite-binary-trees}}

\begin{spec}
data Tree a where
  Leaf    :: Tree a
  Branch  :: Tree a -> a -> Tree a -> Tree a

tree  :: Tree
tree  = Branch tree 47 tree
\end{spec}

\begin{tikzpicture}[%
list/.style={rectangle split, rectangle split parts=3, draw, rectangle split horizontal},>=stealth, start chain]
  \node[list, on chain] (A) { \phantom{1} \nodepart{second} 47 \nodepart{third} \phantom{1} };

  \path (A) edge[dotloop left] node {} (A);
  \path (A) edge[dotloop right] node {} (A);
\end{tikzpicture}

\hypertarget{self-referential-definitions}{%
\subsection{Self-referential
definitions}\label{self-referential-definitions}}

\begin{code}
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
\end{code}


\hypertarget{tying-the-knot}{%
\subsection{Tying the knot}\label{tying-the-knot}}

How do you have two-way links without mutable references? Fixed points!

\begin{code}
data SomeTree where
  ATree :: Maybe SomeTree -> Maybe SomeTree -> Maybe SomeTree -> SomeTree

tree :: SomeTree
tree =
  let  parent  = ATree Nothing (Just left) (Just right)
       left    = ATree (Just parent) Nothing Nothing
       right   = ATree (Just parent) Nothing Nothing
  in parent
\end{code}


\hypertarget{problems}{%
\section{Problems}\label{problems}}

\hypertarget{what-does-this-do}{%
\subsection{What does this do?}\label{what-does-this-do}}

\begin{spec}
foo = ... :: [String]
bar = map f foo
\end{spec}

When we ``fully evaluate'' @bar@:

\begin{enumerate}
\item
  How many times will we apply @f@?
\item
  How much memory will we allocate?
\item
  What shape is the resulting data structure?
\item
  Will it even terminate?
\item
  Can you tell before hand?
\end{enumerate}

\hypertarget{who-knows}{%
\subsection{Who knows?}\label{who-knows}}

Even when we know that @foo@ is in normal form, that @map@
is just @map@, and that @f@ is just the identity function,
we don't know whether @map id foo@ will terminate, whether the
result (in normal form) will fit in memory, etc.

Similar problems occur with @fold@s and various other recursive
functions.

\hypertarget{then-answer}{%
\subsection{Some answers}\label{some-answers}}

\begin{enumerate}
\item
  The answer is not ``@f@ is called once per @String@
  value in memory''.
\item
  The answer is not ``@bar@ will use memory proportional to the
  memory used for @foo@''.
\item
  The answer is not ``@bar@ will be the same shape as
  @foo@ with pointers to @Int@s in place of the pointers
  to @String@s''.
\item
  The answer is not ``@foo@ is already completely evaluated and
  fits in finite memory, so of course it will terminate''.
\end{enumerate}

If we think about it a little bit, the answer to most of these
questions is not related to the ``size'' of @foo@ -- to the number
of @Int@s in memory -- but the number of \emph{paths} from
@foo@, following pointers, to @Int@s.

\begin{enumerate}

\item
  The answer is ``once for reach path following pointers from @foo@ to
  a @String@ value''.
\item
  The answer is ``in proportion to the number of paths from @foo@
  to a @String@ value''.
\item
  The answer is ``the same as @foo@ but with all recursion
  completely unfolded''.
\item
  The answer is ``@bar@ will definitely terminate when
  @foo@ is finite and has no value recursion''.
\end{enumerate}

We can make use of Haskell's features (and the features of any language with
similar value recursion features) to get graph-like data structures, but only
if we don't intend to do any of the standard functional programming things to
them.

This seems slightly less than ideal.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

We started with exactly one cons cell and exactly one @Int@. But we have
value recursion.

\begin{tikzpicture}[list/.style={rectangle split, rectangle split parts=2,
    draw, rectangle split horizontal}, >=stealth, start chain]
  \node[list, on chain] (A) {1};
  \path (A) edge[dotloop above] node {} (A);
\end{tikzpicture}

Now we have one cons cell and one @Int@ for each time @map succ@ follows the
@cdr@ pointer. @map@ will follow the @cdr@ pointer an unbounded number of times
so we have an unbounded amount of time and memory demand.

\begin{tikzpicture}[list/.style={rectangle split, rectangle split parts=2,
    draw, rectangle split horizontal},
    wot/.style={*->},
    >=stealth, start chain]
  \node[list, on chain] (A) {2};
  \node[list, on chain] (B) {2};
  \node[list, on chain] (C) {2};
  \node[on chain] (D) {...};

  \path (A) edge[wot] node {} (B);
  \path (B) edge[wot] node {} (C);
  \path (C) edge[wot] node {} (D);
\end{tikzpicture}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

So what? If we define infinite data structures we shouldn't expect to be
able to completely process them in finite time!

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

Let's stop ``fully evaluating'' then.

Maybe we're going to @zipWith (+) selectedSequence inputData@
and one sequence the users can select is @ones@.

No matter how many values are demanded from @ones@, at the end
it'll have allocated exactly one cons cell and exactly one @Int@.

If we do the same with @twos = map succ ones@ it'll allocate
a new cons cell and a new @Int@ value every time @zipWith@
demands the next value.

Even when we stop ``fully evaluating'' recursive values, it's still
displeasing that a trivial operation on a tiny data structure can
changed the time and/or space complexity of using that structure.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

Annoyingly there's no way to really deal with this in Haskell. If you
didn't want corecursive data you shouldn't have picked Haskell.

But the problem occurs in any language with one or more of:

\begin{itemize}
\item
  lazy values
\item
  a fixed point operator
\item
  mutable references
\end{itemize}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\hypertarget{why-am-i-talking-about-this}{%
\section{Why am I talking about
this?}\label{why-am-i-talking-about-this}}

Let's just not do that then!

We can (fast and loose) assume that we don't construct values that have
these problems. Done!

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

But what about recursive structures?

\begin{itemize}

\item
  Streams
\item
  Trees
\item
  Graphs
\end{itemize}

There are plenty of structures where we want to have the recursive
behaviour we just banned.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\hypertarget{problem}{%
\section{Problem}\label{problem}}

The problem isn't \emph{recursion}. It's \emph{implicit} recursion.

References ``back'' into the the same structure aren't bad. We just need
to be able to distinguish between references ``back'' into structure
we've already seen and ``forward'' into new bits of the structure!

Ignoring mutable data this happens when we define a data structure using
a fixed point (either an explicit operator or implicitly in a language
like Haskell).

The problem arose when we encounter a fixed point in the data structure
and handle it as though it is any old point.

\hypertarget{solution}{%
\section{Solution}\label{solution}}

So let's find a way to represent and manipulate fixed points in our data
structures appropriately.

We could do it by adding a @Loop@ constructor to @Stream@
and @Tree@ and @Graph@ and \ldots{}

But that seems a bit boring (and laborious) so instead we'll use a
generic representation based on parametric higher order abstract syntax
(PHOAS) to handle the tricky bits together with some simple functors to
specialise it to represent streams, trees, graphs, etc.

\hypertarget{free}{%
\section{Free}\label{free}}

Here is the @Free@ data type from Haskell's standard library.


\begin{spec}
data Free f a where
  Pure  :: a             ->  Free f a
  Free  :: f (Free f a)  ->  Free f a
\end{spec}

\hypertarget{free-the-trees}{%
\section{Free the trees}\label{free-the-trees}}

When @f@ is an algebraic data type and we obey our stricture
about ``not doing that then'', values of type @Free f a@ are
trees:

\begin{itemize}
\item
  The leaves have an @a@ value wrapped in a @Pure@
  constructor.
\item
  The nodes have an @f@ of sub-trees wrapped in a @Free@
  constructor.
\end{itemize}

This gives us a ``definitely not loopy'' structure -- they are all
tree-ish -- and we can control the branching by our choice of @f@.

\begin{code}
data ListF a r where
  Cons :: a -> Maybe r -> ListF a r

data BinaryTreeF a r where
  Branch :: r -> a -> r  ->  BinaryTreeF a r

data RoseTreeF a r where
  Node :: a -> [r] -> RoseTreeF a r

data TwoThreeTreeF a r where
  Two    :: r -> a -> r            ->  TwoThreeTreeF a r
  Three  :: r -> a -> r -> a -> r  ->  TwoThreeTreeF a r
\end{code}

\hypertarget{free-variables}{%
\section{Free variables}\label{free-variables}}

It's worth thinking about what we are about. We want:

\begin{enumerate}
\item
  Data structures that are tree-ish (i.e. nice to process); and
\item
  maybe carrying values at certain places in the structure; and
\item
  containing references to other parts of the structure; and
\item
  maintaining some structural invariants when making these references.
\end{enumerate}

What does that last bit mean? We want references that
\textbf{definitely} point to another part of the structure. Which is to
say it's impossible to ``forge'' these recursive references.

\section{Higher-order abstract syntax}

{\it Higher-order abstract syntax} is a technique for representing
  binding forms in an abstract syntax tree with binding forms in the host
  language (e.g. we use functions in our ASTs to represent function bodies)

\begin{spec}
data Term where
  App  ::  Term -> Term    ->  Term
  Abs  ::  (Term -> Term)  ->  Term
\end{spec}

But notice that the host-language functions we use here are too powerful - they
can scrutinise the parameter and return a different @Term@ depending on the argument
passed in.

We wanted to use the host language binders to avoid having to write all the variable
book keeping ourselves and accidentally the whole Haskell into the embedded language!
This is a pretty big escape hatch!

\section{Parametric higher-order abstract syntax}

As we're all very familiar with by now - one way to stop code from inspecting,
constructing or otherwise interfering with parameters you pass them is to make them
polymorphic. Hence: parametric higher-order abstract syntax. We add a constructor
and add a type parameter for (each, distinct sort of) variables.

%{
%format . = "."

\begin{code}
data PTerm v where
  Var  ::  v                   ->  PTerm v
  App  ::  PTerm v -> PTerm v  ->  PTerm v
  Abs  ::  (v -> PTerm v)      ->  PTerm v

newtype Term = Wrap { unwrap :: forall v. PTerm v }

idFunction = Wrap (Abs $ \x -> Var x)
\end{code}
%}

We get to reuse the host language binders without allowing all the host language
capabilities.

\hypertarget{adding-recursion-for-free}{%
\section{Adding recursion for Free}\label{adding-recursion-for-free}}

Now that we know about PHOAS, we can add an explicit recursion operator to @Free f a@
and it is starting to look pretty useful!

\begin{spec}
return  :: Monad m =>  a              -> m a
join    :: Monad m =>  m (m a)        -> m a
mfix    :: Monad m =>  (a -> m a)     -> m a
\end{spec}

%{
%format . = "."
\begin{code}
data Free f a where
  Pure  ::  a                         ->  Free f a
  Free  ::           f (Free f a)     ->  Free f a
  Fix   :: (a    ->  f (Free f a))    ->  Free f a

newtype Graph f = Hide { reveal :: forall a. Free f a }
\end{code}
%}

@Free@ takes an @f@ of sub-structures and wraps them up as a @Graph@. This is where our choice of
@f@ allows us to carry data, to control the recursive structure, etc.

@Pure@ is a variable referencing some other part of the graph. The only way you can get an @a@ is
to be passed one inside the @Fix@ function - and the only thing you can {\it do} with an @a@ is give
it to @Pure@.

And, finally, @Fix@ allows us to introduce recursion by supplying a function which takes a polymorphic
variable token/witness/name and return a structure which might or might not actually use it. But if it
*does* use the variable token/witness/name thingo the only thing it can do is pass it to @Pure@.

We can wrap the thing up with a newtype to force the ``variable'' type parameter to stay polymorphic.

\section{Streams}

\begin{code}
data StreamF a r where
  Step :: a -> r -> StreamF a r
  deriving (Functor, Foldable, Traversable)

instance Bifunctor StreamF where
  bimap f g (Step a r) = Step (f a) (g r)

type Stream a = Graph (StreamF a)

-- 111...
ones :: Stream Int
ones = Hide $ Fix (\loop -> Step 1 (Pure loop))

-- 1222...
one'twos :: Stream Int
one'twos = Hide $ Free (Step 1 (Fix (\loop -> Step 2 (Pure loop))))

-- 121212...
onetwo's :: Stream Int
onetwo's = Hide $ Fix (\loop -> Step 1 (Free (Step 2 (Pure loop))))
\end{code}

\section{Binary trees}

\begin{code}
data TreeF a r where
  Empty  ::                 TreeF a r
  Fork   :: a -> r -> r ->  TreeF a r
  deriving (Functor,Foldable,Traversable)

type Tree a = Graph (TreeF a)

fixedTree :: Tree Int
fixedTree = Hide (Fix $ \loop -> Fork 12 (Pure loop) (Pure loop))
\end{code}

\section{@map@}

We can implement @map@ in so that we don't throw away our structure. First
remember that the values we want to modify are "inside" the values of our @f@
functor type we used with @Free f a@. If we want to replace the @String@s in a
tree with their lengths we'll need something a bit like this:

\begin{spec}
gmap :: (String -> Int) -> Graph (TreeF String) -> Graph (TreeF Int)
\end{spec}

Recall that @f a@ and @f b@ are functors and have kind @* -> *@. So what we
seem to be doing here is lifting a function @String -> Int@ to a natural
transformation and then applying it to the functor "inside" the Free.

%{
%format . = "."
\begin{code}
type f ~> g = forall a. f a -> g a
\end{code}
%}

Once we have a natural transformation, here is how we can apply it to transform
a free the source functor into the equivalent structure over the second:

\begin{code}
transform  :: (Functor f, Functor g)
           => (f ~> g)
           -> Graph f
           -> Graph g
transform f x = Hide (hmap f (reveal x)) where
    hmap :: forall a f g. (Functor f, Functor g) => (f ~> g) -> Free f a -> Free g a
    hmap f (Pure  x)  = Pure x
    hmap f (Fix   g)  = Fix ((f . fmap (hmap f)) . g)
    hmap f (Free  x)  = Free (f (fmap (hmap f) x))
\end{code}

(Most of the type signature noise there is because I turned @-XGATDs@ on to get
the better ADT syntax. Sorry.)

We can use the function-to-be-mapped to construct a natural transformation by
realising that all of the compatible functors must be bifunctors (i.e. they
have {\it two} type parameters, both covariant). If it only had one there would
be data in the data structure for us to map over!

\begin{code}
class Bifunctor p where
  bimap :: (a -> c) -> (b -> d) -> p a b -> p c d
\end{code}

The natural transformation we want applies the function in the first argument
and leaves the second argument alone (that's the recursive argument used by
@Free@). This is exactly @bimap f id@:

\begin{code}
gmap  :: (Bifunctor f, Functor (f a), Functor (f b))
      => (a -> b)
      -> Graph (f a) -> Graph (f b)
gmap f = transform (bimap f id)
\end{code}

Now we can @gmap@ safe in the knowledge that the only thing we'll modify while we
map the values, the structure will stay the same.

|map succ one'twos|

\section{@fold@}

We can similarly implement a framework of @fold@ functions to handle just the sort
of folding we want.

In the generic case we provide functions to handle the references, recursion,
and branching cases (i.e. the three constructors of @Free@):

\begin{code}
gfold :: Functor f => (t -> c) -> ((t -> c) -> c) -> (f c -> c) -> Graph f  ->  c
gfold v l f = trans . reveal
  where
    trans (Pure x) = v x
    trans (Free fa) = f (fmap trans fa)
    trans (Fix g) = l (f . fmap trans . g)
\end{code}

We can use @fold@ to reduce a structure without following the recursive
references. This allows us to compute with the finite parts of a structure
with value recursion.

\begin{code}
fold :: Functor f  => (f c -> c) -> c -> Graph f  -> c
fold alg k = gfold id (\g -> g k) alg
\end{code}

\begin{spec}
streamF2list :: StreamF a [a] -> [a]
streamF2list (Step x xs) = x : xs

> fold streamf2list [] onetwo's 
[1,2]
\end{spec}

When we want to unroll recusion while reducing structures we can use a cyclic
fold. This time we don't need to supply a "zero" element to replace the references,
we'll just follow them!

\begin{code}
cfold :: Functor f  => (f t -> t) -> Graph f  -> t
cfold = gfold id fix
\end{code}

\begin{spec}
> cfold streamf2list onetwo's
[1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,1,2,....
\end{spec}

You can also implement additional special folds. Here's one that can be used
(with appropriate functions)

\begin{code}
sfold :: (Eq t, Functor f) => (f t -> t) -> t -> Graph f -> t
sfold alg k = gfold id (fixVal k) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v
\end{code}

\section{Conclusion}

There's a lot more detail about this approach to graphs (including the ``add recursion to
each structure'' step that I skipped over) in \cite{Oliveira:2012:FPS:2364527.2364541}.

For more details on parametic higher-order abstract syntax, see \cite{Chlipala:2008:PHA:1411204.1411226}

\section*{Bibliography}

\printbibliography

\end{document}
