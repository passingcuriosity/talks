{-# LANGUAGE StandaloneDeriving, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, FlexibleInstances, GADTs, KindSignatures, TypeOperators, MagicHash, DataKinds #-}

module Main where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Type.Equality
import           Data.Vector (Vector)
import qualified Data.Vector as V

type Z = Integer
type R = Double

data Interval (k :: Nat) where
  D :: Z -> Interval 0
  I :: Z -> Interval 1

deriving instance Eq (Interval k)
deriving instance Show (Interval k)

-- | Elementary cubes are products of one or more elementary intervals.
data Cube (d :: Nat) (k :: Nat) where
  -- | A one-dimensional cube
  Cube :: KnownNat k => Interval k -> Cube 1 k
  -- | A higher-dimensional cube
  Times :: (KnownNat k, KnownNat d', KnownNat k') => Interval k -> Cube d' k' -> Cube (1 + d') (k + k')

instance Eq (Cube d k) where
  (Cube k1) == (Cube k2) = k1 == k2
  (Times c1 (k1 :: Cube d1 k1)) == (Times c2 (k2 :: Cube d2 k2) ) = 
    case (c1, c2) of
      (D l1, D l2) | l1 == l2 ->
        case (sameNat (Proxy :: Proxy d1) (Proxy :: Proxy d2), sameNat (Proxy :: Proxy k1) (Proxy :: Proxy k2)) of
          (Just Refl, Just Refl) -> k1 == k2
          _  -> False
      (I l1, I l2) | l1 == l2 ->
        case (sameNat (Proxy :: Proxy d1) (Proxy :: Proxy d2), sameNat (Proxy :: Proxy k1) (Proxy :: Proxy k2)) of
          (Just Refl, Just Refl) -> k1 == k2
          _  -> False
      otherwise -> False

-- | Chains are formal sums of elementary cubes.
data Chain (d :: Nat) (k :: Nat) where
  Empty :: Chain d k
  Chain :: R -> Cube d k -> Chain d k -> Chain d k

-- | The scalar product of two chains.
scalarProduct :: Chain d k -> Chain d k -> R
scalarProduct Empty _ = 0
scalarProduct _ Empty = 0
scalarProduct (Chain a k1 c1) (Chain b k2 c2)
  | k1 == k2 = (a * b) + scalarProduct c1 c2
  | otherwise = scalarProduct c1 c2

boundary :: Chain d (k + 1) -> Chain d k
boundary _ = error "nah"

main :: IO ()
main = putStrLn "yes"
