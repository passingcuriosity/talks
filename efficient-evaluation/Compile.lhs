%if False
\begin{code}
module Compile where

import           Control.Monad.State.Strict
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe

import qualified LC as LC
import qualified DB as DB

type Nat = Int
type Name = String
\end{code}
%endif

\section{Compiling named terms to indexed terms}

We can convert closed terms of a named $\lambda$-calculus to terms with de
Bruijn indices by traversing the term with a map of names to indicies.

\begin{code}
compile :: LC.Term -> Either [String] DB.Term
compile t = Right (evalState (comp t) mempty)
  where
    comp :: LC.Term -> State (Map Name Nat) DB.Term
    comp (LC.Var n)    = (DB.Var . fromJust) <$> (gets (M.lookup n))
    comp (LC.Abs n v)  = DB.Abs <$> (scoped n (comp v))
    comp (LC.App m n)  = DB.App <$> comp m <*> comp n

scoped :: Name -> State (Map Name Nat) a -> State (Map Name Nat) a
scoped n m = do
  s <- get
  put (M.insert n 1 (M.map (+1) s))
  r <- m
  put s
  return r
\end{code}
