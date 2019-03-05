%if False
\begin{code}
module LC where

import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map

type Name = String

\end{code}
%endif

\section{A $\lambda$-calculus with names}

We're probably all familiar with the $\lambda$-calculus by now. Here's a standard
presentation.

Each @Term@s of the $\lambda$-calculus is either:

\begin{itemize}
\item A variable
\item A abstraction
\item An application
\end{itemize}

In Haskell we'll represent this with an ADT that looks a little bit like this
(though a real implementation will have more bits and pieces stuffed into each
node):

\begin{code}
data Term
  =  Var  { varName  :: Name }
  |  Abs  { absName  :: Name,  absBody  :: Term }
  |  App  { appFun   :: Term,  appArg   :: Term }
  deriving (Show, Eq, Ord)
\end{code}

\subsection{Evaluation by substitution}

We can evaluate these terms by substitution -- we evaluate an application to
the body of the function with all free instances of the bound variable replaced
by the argument value.

\[
M N \rightarrow^{*} (\lambda x. B) N \rightarrow B[x := N]
\]

We can implement this evaluation strategy in Haskell. First, we'll need a
substitution operator:

\begin{code}
subst :: Name -> Term -> Term -> Term
subst tgt val = rec
  where
    rec term = case term of
      Var name       | tgt == name  -> val
      Var name       | otherwise    -> term
      Abs name body  | tgt == name  -> term
      Abs name body  | otherwise    -> Abs name (rec body)
      App fun arg                   -> App (rec fun) (rec arg)
\end{code}

Then evaluation by substitution is very straightforward:

\begin{code}
eval :: Term -> Either String Term
eval term = case term of
  Abs _ _                  -> Right term
  Var name                 -> Left ("Free variable: " ++ name)
  App (Abs name body) arg  -> eval (subst name arg body)
  App fn arg               -> eval fn >>= \fn' -> eval (App fn' arg)
\end{code}

This is seems straightforward and looks like it'll work perfectly well but there's
a bug in that code and, even if it was correct, it's also very inefficient: every
time evaluate an application, you rewrite the leaves of the tree. You can improve
constant factors (e.g. skipping sub-terms that do not contain the target variable)
but this is still fundamentally inefficient.
