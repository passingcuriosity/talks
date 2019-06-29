\begin{code}

module Main where

import qualified LC
import qualified DB
import Compile
import qualified Krivine.Normal as Normal
import qualified Krivine.Eager as Eager


type Result e t = ([t], Either e t)

type Eval e t = t -> Result e t



repl :: (Show e) => String -> (Eval e t) -> IO ()
repl prompt eval = loop
  where
    shouldQuit ""      = True
    shouldQuit "exit"  = True
    shouldQuit "quit"  = True
    shouldQuit _       = False
    loop = do
      putStrLn prompt
      hFlush stdout
      line <- getLine
      when not (shouldQuit line) $ do
        
        
main :: IO ()
main = putStrLn "hello"


\end{code}
