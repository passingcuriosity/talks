%if False
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
module Krivine where

import           Control.Monad.State.Strict
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe

import DB

type Nat = Int
type Name = String
\end{code}
%endif

\newcommand{\mconfig}[3]{\langle #2 \:\vert\: #1 \:\vert\: #3 \rangle}
\newcommand{\lvar}[1]{\mathbb{#1}}
\newcommand{\labs}[1]{\lambda.#1}
\newcommand{\lapp}[2]{(#1 \: #2)}
\newcommand{\mclosure}[2]{[#2]#1}

\section{The Krivine machines}

Krivine machines are a family of abstract machines for the efficient evaluation
of $\lambda$-calculi. The exact structure of the machine depends on the language
features, evaluation semantics, etc. First we'll look at a simple machine for
lazy evaluation (like the evaluator we looked at previously).

The a configuration of the machine has three parts:

\begin{itemize}
\item a {\it term} to be evaluated
\item an {\it environment} for that term
\item a {\it stack} of suspended terms
\end{itemize}

This is a lazy machine -- the {\it stack} and {\it environment} contain
{\it closures} rather than terms. A closure pairs an unevaluated term and a copy
of the environment as it stood when that term was captured to the stack or
environment.

Each item in the {\it environment} and the {\it stack} is a closure containing
an unevaluated term and the environment needed to evaluate that term. These are
more or less equivalent to Haskell's ``thunks'' (excepting that these closures
cannot be updated with the result and aren't shared).

\begin{code}
data Closure = C
  {  closureTerm  :: Term
  ,  closureEnv   :: [Closure]
  }

type Config = (Term, [Closure], [Closure])
\end{code}

%if False
\begin{code}
deriving instance Show Closure
\end{code}
%endif

We'll evaluate an application by stashing the argument sub-term in the {\it stack}
of closures and continuing evaluation with the function sub-term.

$$
\frac{
  \mconfig{\rho}{\lapp{M}{N}}{\sigma}
}{
  \mconfig{\rho}{M}{\mclosure{N}{\rho} : \sigma}
}
$$

We'll evaluate an abstraction by moving the most-recently suspended closure from
the {\it stack} to the {\it environment} and continue evaluation with the
function body {\it term}.

$$
\frac{
  \mconfig{\rho}{\labs{M}}{u : \sigma}
}{
  \mconfig{u : \rho}{M}{\sigma}
}
$$

We'll evaluate a variable by dropping that many closures from the front of the
{\it environment} and continuing evaluation with the last of them. (E.g. for
variable $\mathbb{1}$ we'll drop one item and continue evaluation with it,
for $\mathbb{2}$ we'll drop two and continue with the second of them, etc.)

$$
\frac{
  \mconfig{\mclosure{t}{v} : \rho}{\lvar{1}}{\sigma}
}{
  \mconfig{v}{t}{\sigma}
}
$$

$$
\frac{
  \mconfig{v : \rho}{\lvar{N+1}}{\sigma}
}{
  \mconfig{\rho}{\lvar{N}}{\sigma}
}
$$

Notice that these four rules match every possibility for the term to be evaluated
(every term is either an abstraction, an application, the variable $\mathbb{1}$, or
a ``higher'' variable).

Every configuration not matched by these rules is either terminating (see below)
or invalid.

I'm not going to try to prove it but we should be able to convince ourselves that:

\begin{enumerate}
\item any evaluation from a closed term will not lead to an invalid configuration
\item any evaluation from a closed term with a normal form will terminate
\end{enumerate}

The Haskell implementation of these rules is a simple mattern match:

\begin{code}
step :: Config -> Either String Config
step c = case c of
  (App m n  ,              p  ,        s)  ->  Right (m          ,        p  ,  (C n p)  :  s)
  (Abs m    ,              p  ,  u  :  s)  ->  Right (m          ,  u  :  p  ,              s)
  (Var 1    ,  (C t v)  :  _  ,        s)  ->  Right (t          ,        v  ,              s)
  (Var n    ,  _        :  p  ,        s)  ->  Right (Var (n-1)  ,        p  ,              s)
  c                                        ->  Left $ "Encountered invalid configuration: " ++ show c
\end{code}

