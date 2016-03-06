-- {-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

-- import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = [ (x,y) | x <- xs, y <- ys ]
