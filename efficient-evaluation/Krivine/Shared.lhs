%if False
\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
module Krivine.Shared where

import DB
\end{code}
%endif

\begin{code}
data Closure = C
  {  closureTerm  :: Term
  ,  closureEnv   :: [Closure]
  }
\end{code}

%if False
\begin{code}
deriving instance Show Closure
\end{code}
%endif
