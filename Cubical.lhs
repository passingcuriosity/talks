\section{Cubical homology}

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

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
import           Data.Vector (Vector)
import qualified Data.Vector as V

\end{code}
%endif

\subsection{Intervals and cubes}

The basic building block of cubical sets are {\it intervals}. Our intervals are
quite simple:

\begin{description}
\item[Degenerate intervals] are points $[l, l]$.
\item[Non-degenerate intervals] are unit-length closed intervals $[l, l+1]$.
\end{description}

 For integer coordinates $l \in \mathbb{Z}$.

\begin{code}
type Z = Integer
type R = Double

data Interval (k :: Nat) where
  D :: Z -> Interval 0
  I :: Z -> Interval 1

deriving instance Eq (Interval k)
deriving instance Show (Interval k)
\end{code}

\begin{code}
data Cube (d :: Nat) (k :: Nat) where
  Cube :: KnownNat k => Interval k -> Cube 1 k
  Times :: (KnownNat k, KnownNat d', KnownNat k') => Interval k -> Cube d' k' -> Cube (1 + d') (k + k')

instance Eq (Cube d k) where
  (Cube k1) == (Cube k2) = k1 == k2
  (Times c1 (k1 :: Cube d1 k1)) == (Times c2 (k2 :: Cube d2 k2) ) = 
    case (c1, c2) of
      (D l1, D l2) | l1 == l2 ->
        case (sameNat (Proxy :: Proxy d1) (Proxy :: Proxy d2), sameNat (Proxy :: Proxy k1) (Proxy :: Proxy k2)) of
          (Just Refl, Just Refl) -> k1 == k2
          _  -> False
      (I l1, I l2) | l1 == l2 ->
        case (sameNat (Proxy :: Proxy d1) (Proxy :: Proxy d2), sameNat (Proxy :: Proxy k1) (Proxy :: Proxy k2)) of
          (Just Refl, Just Refl) -> k1 == k2
          _  -> False
      otherwise -> False
\end{code}

\subsection{Chains}

\begin{code}
infixr 6 :+

data Chain (d :: Nat) (k :: Nat) where
  Empty :: Chain d k
  (:+) :: (R, Cube d k) -> Chain d k -> Chain d k

cubes :: Chain d k -> [(R, Cube d k)]
cubes (k :+ c) = k : cubes c
cubes Empty    = []

scalarProduct :: Chain d k -> Chain d k -> R
scalarProduct Empty _  = 0
scalarProduct _ Empty  = 0
scalarProduct c1 c2  =
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