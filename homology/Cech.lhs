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
module Cech where

import           Data.Vector (Vector)
import qualified Data.Vector as V

\end{code}
%endif

We indend to study the homology of sets of data points. The first step is to
find a topological structure which corresponds to the set of points.


\begin{code}


type Complex = Vector (Vector Double)

complex :: Set Double -> Complex Double
complex

rips :: Double -> Set Double -> [Complex m]
cech epsilon points =
  let r = epsilon / 2
  in map complex $ filter (pred r) $ Set.subsets points
  where
    -- Check that 
    pred :: Double -> Set Double -> Bool
    pred r points = False
\end{code}
