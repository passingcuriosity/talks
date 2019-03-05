{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Problem where

import Prelude hiding ((>>=), (>>), return)

import IxMonad

data Rel = LT | LTE | EQ | GTE | GT
data Dir = Min | Max

data Spec (m :: Nat) (o :: Bool) (n :: Nat) (p :: Bool) where
    -- | Extend the problem specification with a new variable.
    Var :: String -> Spec m o (m + 1) o (Expr v)
    -- | Extend the problem specification with a new constraint.
    Cstr :: Rel -> Expr v -> Double -> Spec m t m t ()
    -- | Extend the problem specification with an objective function.
    Obj :: Dir -> Expr v -> Spec m False m True ()

var name = Var

constrain :: Expr -> Rel -> n -> Spec  ()

optimise :: Dir -> Expr -> Spec n False n True ()
