data Operation a where
  Dab     :: Operation a
  Krump   :: Int -> Operation a
  Shuffle :: [a] -> Operation a
  Robot   :: Repr a => a -> Operation a
