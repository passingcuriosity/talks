%if False
\begin{code}
module LC.Fmt where

import LC

\end{code}
%endif

\section{Parsing $\lambda$-calculus terms with names}

\begin{code}
type Ctx = ([Name], [Name])

type Parser = Parsec String Ctx Term

getScope :: Parsec String Ctx [Name]
getScope = fst <$> getState

modifyScope :: ([Name] -> [Name]) -> Parsec String Ctx ()
modifyScope f = modifyState (\(x,y) -> (f x, y))

parseName :: Parsec String u Name
parseName = many1 (letter <|> char '\'')

parseAbs :: Parser -> Parser
parseAbs parseTerm = do
  char '\\'
  spaces
  v <- parseName
  modifyScope (v:)
  spaces
  char '.'
  spaces
  term <- parseTerm
  modifyScope tail
  pos <- getPosition
  return (Abs (loc pos) v term)

parseVar :: Parser
parseVar = do
  v <- parseName
  (scope, syms) <- getState
  case (elemIndex v scope, elemIndex v syms) of
    (Just n, _) -> do
      pos <- getPosition
      return (Var (loc pos) v)
    (Nothing, Just n) -> do
      pos <- getPosition
      return (Sym (loc pos) v)
    _ -> fail ("Variable `" ++ v ++ "' is not in scope.")

parseTerm :: Parser
parseTerm = chainl1 parts (space >> getPosition >>= (return . App . loc))
  where
    parens :: Parsec String u a -> Parsec String u a
    parens = between (char '(') (char ')')
    parts :: Parser
    parts = parens parseTerm <|> parseAbs parseTerm <|> parseVar

parse :: String -> Either [String] Term
parse = parseWithSymbols []

parseWithSymbols :: [Name] -> String -> Either [String] Term
parseWithSymbols syms =
  either (Left . (:[]) . show) (Right . id) .
  runParser parseTerm ([], syms) "<INPUT>"
\end{code}

\section{Printing $\lambda$-calculus terms with names}

\begin{code}
format :: Term -> String
format = fmt 0
  where
    fmt _ (Var n) = n
    fmt p (Abs n t) =
      let b = mconcat ["\\", n, ".", fmt 1 t]
      in if p > 1 then "(" ++ b ++ ")" else b
    fmt p (App m n) =
      let b = mconcat [fmt 2 m, " ", fmt 2 n]
      in if p >= 2 then "(" ++ b ++ ")" else b
\end{code}
