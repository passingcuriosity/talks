%if False
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
module Krivine.Eager where

import           Control.Monad.State.Strict
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe

import DB
import Krivine.Shared

type Nat = Int
type Name = String
\end{code}
%endif

\newcommand{\keager}[3]{\langle #1 \:\vert\: #2 \:\vert\: #3 \rangle}
\newcommand{\kleft}{\mathfrak{L}}
\newcommand{\kright}{\mathfrak{R}}

\subsection{A Krivine machine for eager, applicative order evaluation}

We might prefer that our lambda calculus programs be evaluated eagerly. One
machine which can achieve this goal has a similar structure but decorates the
term and stack elements to capture information about the ``stage'' of evaluation
being worked.

The only part of our language which really needs this is an application node.
When evaluating an application we'll need to:

\begin{itemize}
\item evaluate the left-hand term to normal form
\item evaluate the right-hand term to normal form
\item apply the abstraction from the left-hand result to the value from the right-hand result
\end{itemize}

A configuration for this machine includes:

\begin{enumerate}
\item a {\it term} decorated with @Maybe@ (which signifies there's @Nothing@ left to do in this stage)
\item an {\it environment} for the current term
\item a {\it stack} decorated with information the current ``stage''
\end{enumerate}

\begin{code}
data Sided a = L | V a | R

type Config = (Maybe Term, [Closure], [Sided Closure])
\end{code}

%if False
\begin{code}
deriving instance (Show a) => Show (Sided a)
\end{code}
%endif

We'll evaluate an application by stuffing the argument into the stack with a
marker signifying that we're currently working on the left and continuing
evaluation with the left-hand subterm.

\[
\frac{\keager{\lapp{M}{N}}{\rho}{\sigma}}{\keager{M}{\rho}{\kleft : \mclosure{N}{\rho} : \sigma}}
\]

We'll evaluate an abstraction by moving its closure to the stack.

\[
\frac{\keager{\labs{M}}{\rho}{\sigma}}{\keager{\emptyset}{\emptyset}{\mclosure{\labs{M}}{\rho} : \sigma}}
\]

Note what the stack will look like if we've just finished evaluating the
left-hand of an application $\lapp{M}{N}$ has given us a stack that looks like
$\mclosure{\labs{M}}{\rho} : \kleft : \mclosure{N}{\rho} : \sigma$.

We'll evaluate variables in pretty much the same way we did last time.

\[
\frac{\keager{\mathbb{1}}{u : \rho}{\sigma}}{\keager{\emptyset}{\emptyset}{ u : \sigma}}
\]

\[
\frac{\keager{\mathbb{N + 1}}{u : \rho}{\sigma}}{\keager{\mathbb{N}}{\rho}{\sigma}}
\]

When we've finished evaluating the current term then the stack will contain
either:

\begin{itemize}
\item the term we just finished with;
\item a left-hand marker; and
\item the unevaluated right-hand term that we stash when we started with the left
\end{itemize}

or

\begin{itemize}
\item the term we just finished with;
\item a right-hand marker; and
\item the evaluated left-hand term (i.e. an abstraction) we evaluated before continuing with this term from the right
\end{itemize}

The rules look like this:

\[
\frac{
  \keager{\emptyset}{\emptyset}{u : \kleft : \mclosure{n}{\rho} : \sigma}
}{
  \keager{n}{\rho}{\kright : u : \sigma}
}
\]

\[
\frac{
  \keager{\emptyset}{\emptyset}{u : \kright : \mclosure{\labs{M}}{\rho} : \sigma}
}{
  \keager{M}{u : \rho}{\sigma}
}
\]


The Haskell implementation of these rules is, again, a straightforward pattern match:

\begin{code}
step :: Config -> Either String Config
step c = case c of
  (Just (App m n)  ,       p   ,                                      s)  ->  Right (Just m              ,        p   ,  L                :  V (C n p)  :  s)
  (Just (Abs m)    ,       p   ,                                      s)  ->  Right (Nothing             ,        []  ,  V (C (Abs m) p)  :                s)
  (Just (Var 1)    , u  :  p   ,                                      s)  ->  Right (Nothing             ,        []  ,  V u              :                s)
  (Just (Var n)    , u  :  p   ,                                      s)  ->  Right (Just (Var $ n - 1)  ,        p   ,                                    s)
  (Nothing         ,       []  ,  V u  :  L  :  V (C  n        p)  :  s)  ->  Right (Just n              ,        p   ,  R                :  V u        :  s)
  (Nothing         ,       []  ,  V u  :  R  :  V (C  (Abs m)  p)  :  s)  ->  Right (Just m              ,  u  :  p   ,  s)
  c                                                                       ->  Left $ "Encountered invalid configuration: " ++ show c
\end{code}
