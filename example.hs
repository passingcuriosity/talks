{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}

module Main where

data Foo a = Nowt | Summat a (Foo a)

deriving instance Functor Foo

main :: IO ()
main = putStrLn "nowt"
