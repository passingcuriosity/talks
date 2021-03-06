%if False
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
module Cubical where

import           GHC.TypeLits
import           GHC.Exts
import           Data.Proxy
import           Data.Type.Equality
import           Data.Monoid
import           Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (product)

\end{code}
%endif

%options ghci -XRankNTypes -fprint-explicit-foralls

%format Z = "\mathbb{Z}"
%format R = "\mathbb{R}"
%format :~: = "\sim "
%format :*: = " \times "
%format :+ = "{:}{\plus}"

\subsection{Intervals and cubes}

The basic building block of cubical sets are {\it intervals}. Our intervals are
quite simple:

\begin{description}
\item[Degenerate intervals] are points $[l, l]$ for $l \in \mathbb{Z}$.
\item[Non-degenerate intervals] are unit-length closed intervals $[l, l+1]$ for $l \in \mathbb{Z}$.
\end{description}

A {\it cube} is formed by the product of one or more such intervals: $c_1 = i_1 \times ... \times i_n$.
The number of intervals in a cube is its {\it embedding} number and corresponds to the dimension of the
space within which the cube is embedded (i.e. $\mathbb{R}^k$). The number of non-degenerate intervals
within the cube is its {\it dimension}. You might thing of a two-dimensional piece of paper sat on a
table in the three-dimensional space of a library: it might be represented by a cube with embedding
number $3$ and dimension $2$ like, e.g., $[2,2]\times[1,2]\times[8,9]$.

We can track both of these numbers in the types of our cubes. First we'll parameterise our @Interval@
datatype with a natural number tracking ``how many dimensions'' there are in the interval (zero for
degenerate and one for non-degenerate intervals).

\begin{code}
type Z = Integer
type R = Double

data Interval (k :: Nat) where
  Degen  :: Z -> Interval 0
  Inter  :: Z -> Interval 1

deriving instance Eq (Interval k)
deriving instance Show (Interval k)
\end{code}

Along similar lines, we can parameters to our @Cube@ datatype by two natural numbers. The embedding number
can be calculated by tracking the number of intervals in the cube (like the standard approach to
length-indexed vectors) and the dimension can be calculated by adding up the parameters of the intervals
contained in the cube.

\begin{spec}
infixr 6 :*:

data Cube (d :: Nat) (k :: Nat) where
  Cube   ::  Interval k -> Cube 1 k
  (:*:)  ::  (KnownNat k, KnownNat k', KnownNat d', d2 ~ (1 + d'), k2 ~ (k + k'))
         =>  Interval k -> Cube d' k' -> Cube d2 k2
\end{spec}

Alternatively, we can merge the constructors from @Interval k@ and @Cube d k@ into a single datatype to get
slightly nicer syntax without any loss of expressivity or type safety. We can remove the @Cube@ constructor
which embeds an @Interval k@ into @Cube 1 k@ and replace it with the @D@ and @I@ constructors:

\begin{code}
infixr 6 :*:

data Cube (d :: Nat) (k :: Nat) where
  D      :: Z -> Cube 1 0
  I      :: Z -> Cube 1 1
  (:*:)  ::  (KnownNat k, KnownNat k', KnownNat d', d2 ~ (1 + d'), k2 ~ (k + k'))
         =>  Cube 1 k -> Cube d' k' -> Cube d2 k2

deriving instance Show (Cube d k)

fmt :: (KnownNat d, KnownNat k) => Cube d k -> String
fmt (D l) = "[" <> show l <> "]"
fmt (I l) = "[" <> show l <> "," <> show (l + 1) <> "]"
fmt (i :*: c) = fmt i <> " * " <> fmt c
\end{code}

We've replaced types like @Interval k@ with @Cube 1 k@ which has the same values: @Interval 0@ and @Interval 1@
are now @Cube 1 0@ and @Cube 1 1@ which can only be constructed using @D :: Z -> Cube 1 0@ and @I :: Cube 1 1@.
This lets us write cubes straighforwardly in Haskell and format them nicely:

\eval{:t D 1 :*: I 0 :*: D 2}

\eval{putStrLn . fmt $ D 1 :*: I 0 :*: D 2}

To implement equality (and some other binary operations on cubes) we'll need to ensure that they have
matching shapes. They way we've encoded things, this means manually checking that the parts of a cube
still match as we perform our structural recursion. A helper can help reduce the boiler plate here:

\begin{code}
sameSpace  :: (KnownNat d1, KnownNat d2, KnownNat k1, KnownNat k2)
           => Cube d1 k1
           -> Cube d2 k2
           -> Maybe (d1 :~: d2, k1 :~: k2)
sameSpace (c1 :: Cube d1 k1) (c2 :: Cube d2 k2) =
    let  d1  = Proxy :: Proxy d1
         d2  = Proxy :: Proxy d2
         k1  = Proxy :: Proxy k1
         k2  = Proxy :: Proxy k2
    in case (sameNat d1 d2, sameNat k1 k2) of
        (Just p1, Just p2)  -> Just (p1, p2)
        _                   -> Nothing
\end{code}

With this in hand we're able to write the @Eq@ instance for cubes. Two of the same type (i.e. embedding
and dimension) are equal when their first intervals have the same sort and coordinate and rest of the
two cubes are both recursively equal. The only bit of awkwardness is that we need to manually check that
the two remaining cubes have compatible shapes.

\begin{code}
instance Eq (Cube d k) where
  c1 == c2 =
    case (c1, c2) of
      (D l1       , D l2       )  -> l1 == l2
      (I l1       , I l2       )  -> l1 == l2
      (i1 :*: k1  , i2 :*: k2  )  ->
        case (i1, i2, sameSpace k1 k2) of
          (D l1,  D l2,  Just (Refl, Refl))  ->  l1 == l2 && k1 == k2
          (I l1,  I l2,  Just (Refl, Refl))  ->  l1 == l2 && k1 == k2
          otherwise ->  False
      otherwise -> False
\end{code}

We're now also in the position to write the {\it cubical product} operation.

\begin{code}
product  :: ( KnownNat d1, KnownNat d2
            , KnownNat k1, KnownNat k2
            , KnownNat d, KnownNat k
            , d ~ (d1 + d2), k ~ (k1 + k2))
         => Cube d1 k1 -> Cube d2 k2 -> Cube d k
product i@(D _)    c2  = i :*: c2
product i@(I _)    c2  = i :*: c2
product (i :*: c)  c2  = error "TODO: Implement recursive case of cubical product"
\end{code}

\subsection{Chains}

\begin{code}
infixr 6 :+

data Chain (d :: Nat) (k :: Nat) where
  Nil    ::                                Chain d k
  (:+)   :: (R, Cube d k) -> Chain d k ->  Chain d k

cubes :: Chain d k -> [(R, Cube d k)]
cubes Nil       = []
cubes (k :+ c)  = k : cubes c

scalarProduct :: Chain d k -> Chain d k -> R
scalarProduct  Nil    _    = 0
scalarProduct  _      Nil  = 0
scalarProduct  c1     c2   =
  sum  [  a * b
       |  (a,k1)   <-   cubes c1
       ,  (b,k2)   <-   cubes c2
       ,  k1 == k2
       ]
\end{code}

\subsection{Homology}

\subsubsection{Chain groups}

\subsubsection{The boundary operator}

Given a cube, it seems intuatively sensible to talk about its boundary. We can
define an operator that does this: the boundary of a three-dimensional cube 
is the six two-dimensional cubes that make up its faces. The boundary of one of
these two-dimensional cubes is the four one-dimensional cubes (i.e. edges) that
make up its sides. The boundary of one of these one-dimensional cubes is the two
zero-dimensional cubes (i.e. points) that make up its ends.

In each case, the bounday of an $n$-dimensional cube is a collection of 
$(n-1)$-dimensional cubes.

We already have an idea of a "collection" of $m$-dimensional cubes: a chain. So
our boundary operator will produce chains.

\begin{code}
cubeBoundary :: Cube d (k + 1) -> Chain d k
cubeBoundary _ = error "not implemented"
\end{code}

Give that a $k$-chain is a collection of $k$-cubes, we might as well generalise
our boundary operator to apply to $k$-chains too. We can simply apply it to each
of cubes and then add the resulting chains together to give a final result.
Geometrically, we can think of this as allowing ourselves to take the boundary
of both contiguous and non-contiguous shapes.

\begin{code}
boundary :: Chain d (k + 1) -> Chain d k
boundary _ = error "nah"
\end{code}