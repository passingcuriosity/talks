\documentclass[]{article}
%include polycode.fmt

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
            pdftitle={So you think you can map {[}safely{]}},
            pdfauthor={Thomas Sutton},
            pdfborder={0 0 0},
            breaklinks=true}
\urlstyle{same}  % don't use monospace font for urls
\usepackage{color}
\usepackage{fancyvrb}

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
  Dab     ::                      Operation a
  Krump   ::             Int  ->  Operation a
  Shuffle ::             [a]  ->  Operation a
  Robot   :: Repr a =>   a    ->  Operation a
\end{code}

\hypertarget{recursive-types-give-trees}{%
\subsection{\texorpdfstring{Recursive \emph{types} give
trees}{Recursive types give trees}}\label{recursive-types-give-trees}}

We're also familiar with recursive types to define structures which can
contain other instances of themselves: lists, trees, etc.

\begin{code}
data List a where
  Nil   ::                  List a
  Cons  :: a -> List a ->   List a
\end{code}

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
  @let\ rec@ in Ocaml, @val\ rec@ in SML.
\item
  @fix@ in various languages.
\item
  pretty much every binding form in Haskell.
\end{itemize}

\hypertarget{infinite-lists}{%
\subsection{``Infinite'' lists}\label{infinite-lists}}

\begin{code}
ones  :: List Int
ones  = Cons 1 ones
\end{code}

\begin{tikzpicture}[list/.style={rectangle split, rectangle split parts=2,
    draw, rectangle split horizontal}, >=stealth, start chain]
  \node[list, on chain] (A) {47};
  \path (A) edge[dotloop above] node {} (A);
\end{tikzpicture}

\hypertarget{infinite-binary-trees}{%
\subsection{``Infinite'' binary trees}\label{infinite-binary-trees}}

\begin{code}
data Tree a where
  Leaf    :: Tree a
  Branch  :: Tree a -> a -> Tree a -> Tree a

tree  :: Tree
tree  = Branch tree 47 tree
\end{code}

\begin{tikzpicture}[%
list/.style={rectangle split, rectangle split parts=3, draw, rectangle split horizontal},>=stealth, start chain]
  \node[list, on chain] (A) { \phantom{1} \nodepart{second} 12 \nodepart{third} \phantom{1} };

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
data Tree where
  Tree :: Maybe Tree -> Maybe Tree -> Maybe Tree

tree = Tree Nothing (Just left) (Just right)
  where
    left = Tree (Just parent) Nothing Nothing
    right = Tree (Just parent) Nothing Nothing
\end{code}


\hypertarget{problems}{%
\section{Problems}\label{problems}}

\hypertarget{what-does-this-do}{%
\subsection{What does this do?}\label{what-does-this-do}}

\begin{code}
let foo = ... :: [Int]
in map f foo
\end{code}

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
we don't know whether @map\ id\ foo@ will terminate, whether the
result (in normal form) will fit in memory, etc.

Similar problems occur with @fold@s and various other recursive
functions.

\hypertarget{then-answer}{%
\subsection{Then answer}\label{then-answer}}

If we think about it a little bit, the answer to most of the questions
we asked is not related to the ``size'' of @foo@ -- to the number
of @Int@s in memory -- but the number of \emph{paths} from
@foo@, following pointers, to @Int@s.

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

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\begin{enumerate}

\item
  The answer is ``once per path following pointers from @foo@ to
  a @String@ value''.
\item
  The answer is ``in proportion to the number of paths from @foo@
  to a @String@ value''.
\item
  The answer is ``the same as @foo@ but with all recursion
  unfolded''.
\item
  The answer is ``@bar@ will definitely terminate when
  @foo@ is finite and has no value recursion''.
\end{enumerate}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

What?

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

\begin{itemize}
\item
  Haskell is a non-strict language.
\item
  This means every definition is, potentially, self referential.
\item
  This means every recursive \emph{type} allows for recursive
  \emph{value}s.
\item
  But we can't, in general, \emph{observe} this value-level recursion so
  we can't keep from breaking these structures when we map.
\end{itemize}

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}



We have exactly one cons cell and exactly one @Int@. But we have
value recursion.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}


Now we have one cons cell and one @Int@ for each time
@map@ follows the @snd@ pointer. And @map@ will
follow the @snd@ pointer an unbounded number of times.

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

So what? If we define infinite data structures we shouldn't expect to be
able to completely process them in finite time!

\begin{center}\rule{0.5\linewidth}{\linethickness}\end{center}

Let's stop ``fully evaluating'' then.

Maybe we're going to @zipWith\ (+)\ selectedSequence\ inputData@
and one sequence the users can select is @ones@.

No matter how many values are demanded from @ones@, at the end
it'll have allocated exactly one cons cell and exactly one @Int@.

If we do the same with @twos\ =\ map\ succ\ ones@ it'll allocate
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


\begin{code}
data Free f a where
  Pure :: a -> Free f a
  Free :: f (Free f a) -> Free f a
\end{code}

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
tree-ish -- but not the one we're after.

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

\begin{code}
data Term where
  App  ::  Term -> Term    ->  Term
  Abs  ::  (Term -> Term)  ->  Term
\end{code}

But notice that the host-langueg functions we use here are too powerful - they
can scrutinise the parameter and "evaluate" the embedded language differently
depending on the argument form.

We wanted to use the host language binders to avoid having to write all the variable
book keeping ourselves and accidentally the whole Haskell into the embedded language!

\section{Parametric higher-order abstract syntax}

As we're all very familiar with by now - the main way to stop code from inspecting,
forging or otherwise interfering with parameters you pass them is to make them
polymorphic. Hence: parametric higher-order abstract syntax.

%{
%format . = "."

\begin{code}
data PTerm a where
  Var  ::  a                   ->  PTerm a
  App  ::  PTerm a -> PTerm a  ->  PTerm a
  Abs  ::  (a -> PTerm a)      ->  PTerm a

newtype Term = Wrap { unwrap :: forall a. PTerm a }

id = Wrap (Abs $ \x -> Var x)
\end{code}

%}

We're back to explicitly representing variables in our AST, but names can never accidentally conflict

\hypertarget{adding-recursion-for-free}{%
\section{Adding recursion for Free}\label{adding-recursion-for-free}}

%{
%format . = "."

\begin{code}
return  :: Monad m =>  a              -> m a
join    :: Monad m =>  m (m a)        -> m a
mfix    :: Monad m =>  (a -> m  a)    -> m a

data Free f a where
  Pure  ::  a                         ->  Free f a
  Fix   :: (a    ->  f (Free f a))    ->  Free f a
  Free  ::           f (Free f a)     ->  Free f a
  PFix  :: ([a]  ->  [f (Free f a)])  ->  Free f a

newtype Graph f = Wrap { unwrap :: forall a. Free f a }
\end{code}
%}

@Pure@ is a variable referencing some other part of the graph. The only way you can get an @a@ is
to be passed one inside the @Fix@ function - and the only thing you {\it can} do with an @a@ is give
it to @Pure@.

@Free@ takes an @f@-functor of sub-structures and wraps them up as a graph. This is where our choice
of functor allows us to carry any data, to control the recursive structure of the graph, etc.

And, finally, @Fix@ allows us to introduce recursion by supplying a function which takes a polymorphic
variable token/witness/name and return a structure which might or might not actually use it. But if it
*does* use the variable token/witness/name thingo the only thing it can do is pass it to @Pure@.

Thanks polymorphism!

\section{Streams}

\begin{code}
data StreamF a r where
  Cons :: a -> r -> StreamF a r
  deriving (Functor,Foldable,Traversable)

type Stream a = Graph (StreamF a)

ones :: Stream Int
ones = Wrap $ Fix (\loop -> Cons 1 (Pure loop))

one'twos :: Stream Int
one'twos = Wrap $ Cons 1 (Fix (\loop -> Cons 2 (Pure loop)))
\end{code}

\section{Binary trees}

\begin{code}
data TreeF a r where
  Empty  ::                 TreeF a r
  Fork   :: a -> r -> r ->  TreeF a r
  deriving (Functor,Foldable,Traversable)

type Tree a = Graph (TreeF a)

tree :: Tree Int
tree = Fix $ \loop -> Fork 12 (Pure loop) (Pure loop)
\end{code}

\section{@map@}

We can implement @map@ in so that we don't throw away our structure. First remember
that we want to write a function like this:

\begin{code}
gmap :: (a -> b) -> Graph (f a) -> Graph (f b)
\end{code}

And recall that @f a@ and @f b@ are have kind @* -> *@ (because they are functors).
Maybe we can use a natural transformation!

%{
%format . = "."
\begin{code}
type f ~> g = forall a. f a -> f g
\end{code}
%}

\begin{code}
transform  :: (Functor f, Functor g)
           => (f ~> g) -> Graph f -> Graph g
transform f x = Wrap (hmap (unwrap x))
  where
    hmap (Pure x)  = Pure x
    hmap (Free x)  = Free (f (fmap hmap x))
    hmap (Fix g)   = Fix ( (f . fmap hmap) . g)
\end{code}

Because we want to map over graphs build with ``structural'' ADTs with two
type parameters and it's the {\it first} one -- the one that describes the values
we want to modify -- we'll need to use @Bifunctor@.

\begin{code}
class Bifunctor p where
  bimap :: (a -> c) -> (b -> d) -> p a b -> p c d

gmap  :: (Bifunctor f, Functor (f a), Functor (f b))
      => (a -> b) -> Graph (f a) -> Graph (f b)
gmap f = transform (bimap f id)
\end{code}

Now we can @gmap@ safe in the knowledge that the only thing we'll modify while we
map the values, the structure will stay the same.

\begin{code}
map succ one'twos
\end{code}

\section{@fold@}

We can similarly implement a framework of @fold@ functions to handle just the sort
of folding we want:

\begin{code}
gfold :: Functor f => (t -> c) -> ((t -> c) -> c) -> (f c -> c) -> Graph f  ->  c
gfold v l f = trans . unwrap
  where
    trans (Pure x) = v x
    trans (Free fa) = f (fmap trans fa)
    trans (Fix g) = l (f . fmap trans . g)

fold :: Functor f  => (f c -> c) -> c -> Graph f  -> c
fold alg k = gfold id (\g -> g k) alg

cfold :: Functor f  => (f t -> t) -> Graph f  -> t
cfold = gfold id fix

sfold :: (Eq t, Functor f) => (f t -> t) -> t -> Graph f -> t
sfold alg k = gfold id (fixVal k) alg

fixVal :: Eq a => a -> (a -> a) -> a
fixVal v f = if v == v' then v else fixVal v' f
  where v' = f v
\end{code}

\end{document}
