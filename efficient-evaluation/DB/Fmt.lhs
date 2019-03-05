%if False
\begin{code}
module DB.Fmt where

import DB

\end{code}
%endif

\section{Printing $\lambda$-calculus terms with de Bruijn indices}

\begin{code}
format :: Term -> String
format = fmt 0
  where
    fmt _ (Var n) = show n
    fmt p (Abs t) =
      let b = "λ." ++ (fmt 1 t)
      in if p > 1 then "(" ++ b ++ ")" else b
    fmt p (App m n) =
      let b = mconcat [fmt 3 m, " ", fmt 3 n]
      in if p > 2 then "(" ++ b ++ ")" else b
\end{code}
