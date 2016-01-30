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

