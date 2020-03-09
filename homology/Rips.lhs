%if False
\begin{code}
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
module Rips where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Vector (Vector)
import qualified Data.Vector as V

\end{code}
%endif

We indend to study the homology of sets of data points. The first step is to
find a topological structure which corresponds to the set of points.


\begin{code}

type Point = Vector Double

distance :: Point -> Point -> Double
distance p1 p2 = 0

type Complex = Vector Point

complex :: [Point] -> Complex
complex points = mempty

mapCons :: (a -> [a] -> c) -> [a] -> [c]
mapCons f = work
  where
    work [] = []
    work (a:as) = f a as : work as

rips :: Double -> Set Point -> [Complex]
rips epsilon points =
  let r = epsilon / 2
  in map complex . filter (pred r) . map Set.toList . Set.toList $ Set.powerSet points
  where
    pred :: Double -> [Point] -> Bool
    pred r ps = and $ mapCons (\x ys -> all (\y -> (distance x y) < r) ys) ps
\end{code}
