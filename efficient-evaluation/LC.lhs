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
\item A lambda abstraction (i.e. function defininition)
\item An application of one term to another (i.e. a function invocation)
\item A variable
\end{itemize}

\begin{code}
data Term
  =  Var  { varName  :: Name }
  |  Abs  { absName  :: Name,  absBody  :: Term }
  |  App  { appFun   :: Term,  appArg   :: Term }
  deriving (Show, Eq, Ord)
\end{code}

We can evaluate these terms by substitution -- when we pass through a binder
during the evaluation process we replace all free instances of the bound name
in the body with the argument value.

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

Evaluation by substitution is very straightforward:

\begin{code}
eval :: Term -> Either String Term
eval term = case term of
  Abs _ _                  -> Right term
  Var name                 -> Left ("Free variable: " ++ name)
  App (Abs name body) arg  -> eval (subst name arg body)
  App _ _                  -> Left ("Can't apply non-function: " ++ show term)
\end{code}

This is very straightforward and works perfectly well but it's also very
inefficient: every time evaluate an application, you rewrite the leaves of the
tree. You can improve constant factors (e.g. skipping sub-terms that do not
contain the target variable) but this is still fundamentally inefficient.
