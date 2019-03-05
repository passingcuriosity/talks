%if False
\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
module Simplex.DSL where

import GHC.TypeLits
import Prelude hiding (LT, GT, EQ)

\end{code}
%endif

\section{Specifying problems}

\begin{code}
data Var = Var
    deriving (Eq, Show)

data Expr
    = V Var
    | Coeff Double Expr
    | Sum (Expr) (Expr)
    deriving (Eq, Show)

type family Linear (n :: Nat) where
    Linear 0 = Expr
    Linear n = (Var -> Linear (n - 1))
\end{code}

\subsection{Representing optimisation objectives}

We'll need a way to specify a linear function

\begin{code}
data Dir = Max | Min
  deriving (Eq, Show)

data Objective n = Objective 
    { direction :: Dir
    , oFunction :: Linear n
    }

maximise :: KnownNat n => Linear n -> Objective n
maximise = Objective Max

minimise :: KnownNat n => Linear n -> Objective n
minimise = Objective Min
\end{code}

\begin{code}

newtype Constraints n = Constraints (Constraint n, [Constraint n])

data Rel = GT | GEQ | EQ | LEQ | LT

data Constraint n = Constraint
    { relation :: Rel
    , cFunction :: Linear n
    , value :: Double
    }

gt = Objective GT

geq = Objective GEQ

eq = Objective EQ

leq = Objective LEQ

lt = Objective LT

\end{code}

\begin{code}
data Problem n = Problem
    { objective :: Objective n
    , constraints :: Constraints n
    }
\end{code}


\begin{code}

instance Num Expr where
    (+) = \_ _ -> error "+"
    (-) = \_ _ -> error "-"
    (*) = \_ _ -> error "*"

var :: Monad m => String -> m Var
var n = error $ "Need to defined variable: " ++ n

constrainM :: Monad m => Constraint n -> m ()
constrainM c = error $ "Need to constrain"

minimiseM :: Monad m => Expr -> m ()
minimiseM c = error $ "Need to minimise"

problem :: Monad m => m () -> 

prob = do
    a <- V <$> var "a"
    b <- V <$> var "b"
    c <- V <$> var "c"

    constrainM $ ((12 * a - 3 * b) `leq` 100)
    constrainM $ ((3 * b) `geq` 10)
    constrainM $ ((a + b + c) `eq` 0)

    minimiseM $ a + b - c
\end{code}

\begin{example}

solve $ \x y z ->
    minimise $ 12 * x - 3 y + z
    <:> x + y + z `gte` 10
    <&> x + y + z `lte` 20
    <&> x `gte` 0
    <&> y `gte` 10
    <&> y `lte` 100

\end{example}
