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

\subsubsection{$\alpha$-complex}

\subsubsection{Vietoris-Rips complex}

The Vietoris-Rips complex (also the Vietoris complex or Rips complex)

\begin{quote}
defined from any metric space M and distance δ by forming a simplex for every
finite set of points that has diameter at most δ.

If a finite set S has the property that the distance between every pair of
points in S is at most δ, then we include S as a simplex in the complex.
\end{quote}

\subsubsection{\u Cech complex}

The \u Cech complex $\check{C}_{\varepsilon}(X)$ of a pointcloud $X$ and distance
$\varepsilon > 0$

\begin{quote}
Take the elements of X as the vertex set of ${\check {C}}_{\varepsilon }(X)$.
Then, for each $\sigma \subset X$, let
$\sigma \in {\check {C}}_{\varepsilon }(X)$ if the set of ε-balls
centered at points of σ has a nonempty intersection. In other words, the Čech
complex is the nerve of the set of ε-balls centered at points of X. By the nerve
lemma, the Čech complex is homotopy equivalent to the union of the balls.
\end{quote}

The \check{C}ech complex is a sub-complex of the Vietoris-Rips complex.
