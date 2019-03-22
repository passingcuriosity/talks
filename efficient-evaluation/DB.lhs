%if False
\begin{code}
module DB where

type Nat = Int

\end{code}
%endif

\section{A $\lambda$-calculus with de Bruijn indices}

Another style of lambda calculus presentation replaces variable names with
indices into a data structure -- when we encounter a variable (index) we can
tell how many levels "outward" to go to find the binder that introduced that
variable: the variable $\mathbb{1}$ is the variable introducted by first
binder, the variable $\mathbb{2}$ by the next, etc.

\[
  \lambda x. \lambda y. \lambda z. z \equiv \lambda \lambda \lambda 1
\]
\[
  \lambda x. \lambda y. \lambda z. y \equiv \lambda \lambda \lambda 2
\]
\[
  \lambda x. \lambda y. \lambda z. x \equiv \lambda \lambda \lambda 3
\]

\[
  \lambda x. \lambda y. y x \equiv \lambda \lambda \lambda 1 2
\]


\begin{code}
data Term
  = Var Nat
  | Abs Term
  | App Term Term
  deriving (Show, Eq)
\end{code}

If you've heard things described as "nameless" or "locally nameless" this is
the sort of thing they mean.

\subsection{Evaluating}

Like the $\lambda$-calculus with names, we can also evaluate these expressions
using an explicit environment.


Instead of a map associating names to values (@Map Name Closure@ in our case)
 use a vector of values.
