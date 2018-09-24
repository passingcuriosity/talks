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

One of our goals here is to be able to take data sets (which we might expect to
be collections of points in a space) and analyse the shape.

\Todo{Why triangles?}

\subsection{Simplices}

The fundamental building block of simplical homology is the {\it simplex}, or
$n$-dimensional triangle. In general we call an $n$-dimensional simplex an
$n$-simplex but we have special names for simplicies in the few lowest
dimensions:

\begin{itemize}
\item 0-simplices are points
\item 1-simplices are lines
\item 2-simplices are triangles
\item 3-simplices are tetrahedrons
\end{itemize}

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
