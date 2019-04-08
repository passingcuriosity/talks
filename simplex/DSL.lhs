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

\subsection{Specifying problems}

We'll need to describe the objectives and constraints of a problem. Both are
essentially linear functions labelled in various ways, so we'll start with a
way to express linear functions.

\begin{code}

data Expr (n :: Bool) where
    K  ::  Double                ->  Expr False
    V  ::  Double   ->   Var     ->  Expr True
    S  ::  Expr a   ->   Expr b  ->  Expr (Or a b)
\end{code}

We'll be dealing with linear functions throughout, so let's get that figure out
first.

\begin{code}
type NonEmptyList a = (a, [a])

type Var = String
type Fn = NonEmptyList (Double, Var)
\end{code}

The objective for each problem is just a linear function labelled with the
appropriate direction (should we attempt to maximise or minimise the function):

\begin{code}
data Obj = MIN | MAX deriving (Eq, Show)
type Objective = (Obj, Fn)
\end{code}

And each constraint is a relation between two linear functions:

\begin{code}
data Rel = LT | LTE | EQ | GT | GTE | GT deriving (Eq, Show)

type Constraint = (Fn, Rel, Fn)
\end{code}

A problem then is:

\begin{code}
newtype Problem = Problem 
    { objective :: Objective
    , constraints :: NonEmptyList Constraint
    }
\end{code}

We can convert problems into standard form by:

\begin{itemize}
\item simplifying all linear functions
\item transforming all relations to @LEQ@
\item transforming all objectives to @MIN@
\end{itemize}

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

prob1 = do
    a <- V <$> var "a"
    b <- V <$> var "b"
    c <- V <$> var "c"

    constrainM $ ((12 * a - 3 * b) `leq` 100)
    constrainM $ ((3 * b) `geq` 10)
    constrainM $ ((a + b + c) `eq` 0)

    minimiseM $ a + b - c

prob2 = solve $ \x y z ->
    minimise $ 12 * x - 3 y + z
    <:> x + y + z `gte` 10
    <&> x + y + z `lte` 20
    <&> x `gte` 0
    <&> y `gte` 10
    <&> y `lte` 100
\end{code}
