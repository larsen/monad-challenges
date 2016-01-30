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
