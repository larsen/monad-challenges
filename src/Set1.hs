{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Random number generation

-- rand :: Seed -> (Integer, Seed)
-- mkSeed :: Integer -> Seed

-- check: 8681089573064486461641871805074254223660

fiveRands :: [Integer]
fiveRands = [
  fst $ rand $ mkSeed 1,
  fst $ rand $ snd $ rand $ mkSeed 1,
  fst $ rand $ snd $ rand $ snd $ rand $ mkSeed 1,
  fst $ rand $ snd $ rand $ snd $ rand $ snd $ rand $ mkSeed 1,
  fst $ rand $ snd $ rand $ snd $ rand $ snd $ rand $ snd $ rand $ mkSeed 1]

fiveRands' :: [Integer]
fiveRands' = map fst $ take 5 $ iterate (rand . snd) $ rand $ mkSeed 1

-- Random Character generation

-- check: 9d475eb78d3e38085220ed6ebde9d8f7d26540bb1c8f9382479c3acd4c8c94a3

randLetter :: Seed -> (Char, Seed)
randLetter seed = (toLetter $ fst r, snd r)
  where r = rand seed

randString3 :: String
randString3 = map fst $ take 3 $ iterate (randLetter . snd) $ randLetter $ mkSeed 1

-- More Generators

-- WIP (not sure I captured the correct abstraction)

type Gen a = Seed -> (a, Seed)
myRand :: Gen Integer
myRand = rand

myRandLetter :: Gen Char
myRandLetter = randLetter

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g seed = (f $ fst r, snd r)
  where r = g seed

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (\x -> x * 2) myRand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA (\x -> x * 2 + 1) myRand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (\x -> x * 10) myRand

-- Generalizing Random Pairs

randPair :: Gen (Char, Integer)
randPair seed = ((fst l, fst n), snd n)
  where l = randLetter seed
        n = rand $ snd l

generalPair :: Gen a -> Gen b -> Gen (a, b)
-- generalPair :: (Seed -> (a, Seed)) -> (Seed -> (b, Seed)) -> Seed -> ((a,b), Seed)
generalPair gena genb seed = ((a, b), seed'')
  where ra     = gena seed
        a      = fst ra
        seed'  = snd ra
        rb     = genb seed'
        b      = fst rb
        seed'' = snd rb

randPair' = generalPair randLetter rand

-- Generalizing pairs even more

generalB :: Gen a -> Gen b -> (a -> b -> c) -> Gen c
generalB gena genb f seed = (f a b, seed'')
  where ra     = gena seed
        a      = fst ra
        seed'  = snd ra
        rb     = genb seed'
        b      = fst rb
        seed'' = snd rb

generalPair2 = generalB randLetter rand (\a b -> (a, b))
