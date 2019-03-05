%if False
\begin{code}
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

\newcommand{\mconfig}[3]{\langle #1 \:\vert\: #2 \:\vert\: #3 \rangle}
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
\item the current {\it environment}
\item the current {\it term}
\item a {\it stack} of suspended terms
\end{itemize}

Each item in the {\it environment} and the {\it stack} is a closure containing
an unevaluated term and the environment needed to evaluate that term. These are
more or less equivalent to Haskell's "thunks" (excepting that these closures
cannot be updated with the result and aren't shared).

\begin{code}
data Closure = Closure
  {  closureTerm  :: Term
  ,  closureEnv   :: [Closure]
  }

data State = State
  { environment  ::  [Closure]
  , term         ::  Term
  , suspended    ::  [Closure]
  }
\end{code}

We'll evaluate an application by stashing the argument sub-term in the {\it stack}
of closures and continuing evaluation with the function sub-term.

$$
\frac{
  \mconfig{p}{\lapp{M}{N}}{s}
}{
  \mconfig{p}{M}{\mclosure{N}{p} : s}
}
$$

We'll evaluate an abstraction by moving the most-recently suspended closure from
the {\it stack} to the {\it environment} and continue evaluation with the
function body {\it term}.

$$
\frac{
  \mconfig{p}{\labs{M}}{u : s}
}{
  \mconfig{u : p}{M}{s}
}
$$

We'll evaluate a variable by dropping that many closures from the front of the
{\it environment} and continuing evaluation with the last of them. (E.g. for
variable $\mathbb{1}$ we'll drop one item and continue evaluation with it,
for $\mathbb{2}$ we'll drop two and continue with the second of them, etc.)

$$
\frac{
  \mconfig{\mclosure{t}{v}:p}{\lvar{1}}{s}
}{
  \mconfig{v}{t}{s}
}
$$

$$
\frac{
  \mconfig{v : p}{\lvar{N}}{s}
}{
  \mconfig{p}{\lvar{N - 1}}{s}
}
$$

We should be able to convince outselves of two things:

\begin{enumerate}
\item any closed term with a normal form can be evaluated by this machine
\item it's more efficient than it was.
\end{enumerate}
