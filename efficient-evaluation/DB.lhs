%if False
\begin{code}
module DB where

type Nat = Int
type Name = String

\end{code}
%endif

\section{A $\lambda$-calculus with de Bruijn indices}

Another style of lambda calculus presentation replaces variable names with
indices into a data structure -- when we encounter a variable (index) we can
tell how many levels "outward" to go to find the binder that introduced that
variable: the variable $\mathbb{1}$ is the variable introducted by first
binder, the variable $\mathbb{2}$ by the next, etc.

$\lambda x. \lambda y. \lambda z. y = \lambda \lambda \lambda 2$

\begin{code}
data Term
  = Var Nat
  | Abs Term
  | App Term Term
  deriving (Show, Eq)
\end{code}
