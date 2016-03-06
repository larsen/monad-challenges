{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs [] _ = []
allPairs (x:xs) ys = makePairs x ys ++ allPairs xs ys
  where
    makePairs :: a -> [b] -> [(a, b)]
    makePairs _ [] = []
    makePairs x (y:ys) = (x,y) : makePairs x ys
