{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Data.List (minimum, maximum)

-- The Maybe Type

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ show a
  show Nothing  = "Nothing"

-- Build a library of things that fail

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (h:_) = Just h

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:t) = Just t

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay item [] = Nothing
lookupMay item ((k,v):ks)
  | item == k = Just v
  | otherwise = lookupMay item ks

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay n d = Just (n / d)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay l  = Just (maximum l)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay l  = Just (minimum l)

-- Chains of failing computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greekData item =
  case lookupMay item greekData of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just t -> case maximumMay t of
        Nothing -> Nothing
        Just m -> case headMay xs of
          Nothing -> Nothing
          Just h -> divMay (fromIntegral m) (fromIntegral h)

-- check;
-- queryGreek greekDataA "alpha" == Just 2.0
-- queryGreek greekDataA "beta" == Nothing
-- queryGreek greekDataA "gamma" == Just 3.3333333333333335
-- queryGreek greekDataA "delta" == Nothing
-- queryGreek greekDataA "zeta" == Nothing

-- Generalizing chains of failures

-- If you’re really ambitious...

composeMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
composeMaybe m f =
  case m of
    Nothing -> Nothing
    Just res -> f res

(<**>) :: Maybe a -> (a -> Maybe b) -> Maybe b
m <**> f = composeMaybe m f

queryGreek' :: GreekData -> String -> Maybe Double
queryGreek' greekData item =
  case mh of
    Nothing -> Nothing
    Just h -> case mt of
      Nothing -> Nothing
      Just t -> divMay (fromIntegral t) (fromIntegral h)
  where mxs = (lookupMay item greekData)
          <**> (\xs -> Just xs)
        mh = mxs
          <**> (\xs -> headMay xs)
        mt = mxs
          <**> (\xs -> tailMay xs)
          <**> (\xs -> maximumMay xs)

-- Now the actual exercise

-- Considering the signatures, I think link is
-- what i called composeMaybe

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = composeMaybe

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain = flip link

-- Tried this implementation, it's very
-- visible `chain` is the flipped version of `link`
-- chain f m = case m of
--   Nothing -> Nothing
--   Just value -> f value

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greekData item =
  link mh (\x -> link mt (\z -> divMay (fromIntegral z) (fromIntegral x)))
  where mxs = chain (\xs -> Just xs) (lookupMay item greekData)
        mh = chain (\xs -> headMay xs) mxs
        mt = link mxs (\xs -> link (tailMay xs) (\xs -> maximumMay xs)) 

-- OK, I got rid of the `case ... of` occurrences but
-- I'm not satisfied with the syntax

-- Chaining variations

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries p1 p2 =
  link s1 (\s -> link s2 (\s' -> Just (s + s')))
  where s1 = lookupMay p1 salaries
        s2 = lookupMay p2 salaries
