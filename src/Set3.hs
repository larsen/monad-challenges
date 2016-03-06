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

data Card = Card Int String

instance Show Card where
  show (Card rank suit) = show rank ++ suit

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards (x:xs) ys = makeCards x ys ++ allCards xs ys
  where
    makeCards :: Int -> [String] -> [Card]
    makeCards _ [] = []
    makeCards x (y:ys) = (Card x y) : makeCards x ys

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs f (x:xs) ys = makePairs f x ys ++ allCombs f xs ys
  where
    makePairs :: (a -> b -> c) -> a -> [b] -> [c]
    makePairs _ _ [] = []
    makePairs f x (y:ys) = f x y : makePairs f x ys

allPairs' = allCombs makePair
  where makePair x y = (x, y)

allCards' = allCombs (\x y -> Card x y)
