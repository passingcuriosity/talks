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

\section{Simplical homology}

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

\subsubsection{$\alpha$-complex}
