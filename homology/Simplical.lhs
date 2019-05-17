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
module Simplical where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
import           Data.Vector (Vector)
import qualified Data.Vector as V

\end{code}
%endif

One approach to representing more or less arbitrary surfaces for processing on
a computer is to take a discrete approximation. One approach which might be
familiar from 3D graphics (and other applications) is to use triangles. While
the graphics in a compute game needs to model visible surfaces in three
dimensions, we'll be representing objects in higher and lower dimensions so
we'll need to go a bit further than standard triangles: we'll use simplexes.

\subsection{Simplices}

The fundamental building block of simplical homology is the {\it simplex}, or
$n$-dimensional triangle. In general we call an $n$-dimensional simplex an
$n$-simplex but we have special names for simplicies in the lower dimensions
based on our everyday experience:

\begin{itemize}
\item 0-simplices have 1 vertex and are called ``points''
\item 1-simplices have 2 vertices and are called ``lines''
\item 2-simplices have 3 vertices and are called ``triangles''
\item 3-simplices have 4 vertices and are called ``tetrahedrons''
\end{itemize}

As we can see from the pattern above, each $n$-simplex has $n+1$ vertices. We
can think of an $n$-simplex as having an orientation: we can flip the line 
$AB$ to get the line $BA$ and we can flip the triangle $ABC$ to get the triangle
$ACB$.

This leads to our representation of simplices: an $n$-simplex is an ordered set
of $n+1$ vertices where the order of the vertices up to even permutation
determines the orientation of the simplex.

"ABC" "BCA", "CAB" are different perspectives of the same triangle in the same
orientation and "ACB", "CBA", "BAC" are different perspectives on the same
triangle after it has been flipped.

\begin{figure}
\caption{Orientation of 1 and 2-simplices}
\end{figure}

\subsection{Simplical complexes}

\subsection{Building simplical complexes}

We often want to construct a simplical complex from a set of data points. We'll look
at three algorithms that can do just this. They each work by enumerating finite sets
of points and determining whether they are ``close enough'' in some sense or other.
These algorithms range in complexity, so selecting an appropriate algorithm can be
quite important when analysing a large data set.

\subsubsection{Vietoris-Rips complex}

The Vietoris-Rips complex (also the Vietoris complex or Rips complex) on a set of
points contains a simplex corresponding to each finite subset of points

\begin{quote}
defined from any metric space M and distance $\delta$ by forming a simplex for every
finite set of points that has diameter at most $\delta$.

If a finite set S has the property that the distance between every pair of
points in S is at most $\delta$, then we include S as a simplex in the complex.
\end{quote}

\subsubsection{\u Cech complex}

The \u Cech complex $\check{C}_{\varepsilon}(X)$ of a pointcloud $X$ and distance
$\varepsilon > 0$ contains a simplex corresponding to each finite set of points
such that the intersection of the $\varepsilon$-balls centred on each point it not
empty:

$$
\check{C}_{\varepsilon}(X)
:=
\{ \sigma \in \mathcal{P}(X)  || \cap_{p \in \sigma} B_{\varepsilon}(p) \not\equiv \emptyset \}
$$

The \u{C}ech complex is a sub-complex of the Vietoris-Rips complex.

\subsubsection{$\alpha$-complex}

\subsection{Simplical chains}

A {\it simplical $k$-chain} is a formal sum of $k$-simplices:

$$
c_k = \Sigma_{i=1}^{N} c_i\sigma_i
$$

The group of $k$-chains on some simplical complex $S$ is written $C_{k}$ and is
a free Abelian group with a basis in one-to-one correspondence with the set of
$k$-simplices in $S$.

\subsection{Boundaries}

The boundary operator $\partial_{k} : C_k \rightarrow C_{k - 1}$ is the
homomorphism defined by

$$
\partial_{k}(\sigma) = \Sigma_{i=0}^{k} (-1)^{i}(v_0,...,\hat{v_{i}},...,v_{k})
$$

where $(v_0,...,\hat{v_{i}},...,v_{k})$ is the $i$th face of $(v_0,...,v_{i},...,v_{k})$,
obtained by deleting the $i$th vertex of $\sigma$.
