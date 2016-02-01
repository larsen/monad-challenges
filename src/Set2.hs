{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Data.List (minimum, maximum)

-- The Maybe Type

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing  = "_"

-- Build a library of things that fail

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (h:_) = Just h

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:t) = Just t

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay item [] = Nothing
lookup item ((k,v):ks)
  | item == k = Just v
  | otherwise = lookup item ks

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay n d = Just (n / d)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay l  = Just (maximum l)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay l  = Just (minimum l)
