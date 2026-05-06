{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Task2 ( Stream(..), fromList, zipWithStream )
import Data.Ratio (Ratio, numerator)

-- | Power series represented as infinite stream of coefficients
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (x + x ^ 2 + x ^ 4)
-- [0,1,1,0,1,0,0,0,0,0]
-- >>> coefficients ((1 + x)^5)
-- [1,5,10,10,5,1,0,0,0,0]
-- >>> coefficients (42 :: Series Integer)
-- [42,0,0,0,0,0,0,0,0,0]
--
newtype Series a = Series
  { coefficients :: Stream a
  -- ^ Returns coefficients of given power series
  --
  -- For following series
  --   @a0 + a1 * x + a2 * x^2 + ...@
  -- coefficients would be
  --   @a0, a1, a2, ...@
  }

-- | Power series corresponding to single @x@
--
-- First 10 coefficients:
--
-- >>> coefficients x
-- [0,1,0,0,0,0,0,0,0,0]
--
x :: Num a => Series a
x = Series $ fromList 0 [0,1]

instance Num a => Num (Series a) where 
  fromInteger :: Integer -> Series a
  fromInteger n = Series $ fromList 0 [fromInteger n]

  negate :: Num a => Series a -> Series a
  negate (Series s) = Series $ negate <$> s

  (+) :: Series a -> Series a -> Series a
  Series s1 + Series s2 = Series $ zipWithStream (+) s1 s2

  (*) :: Num a => Series a -> Series a -> Series a
  Series s1 * Series s2 = Series $ multiple s1 s2
    where
      multiple :: Num a => Stream a -> Stream a -> Stream a
      multiple (Stream a0 a) b'@(Stream b0 b) = Stream (a0 * b0)
        $ zipWithStream
        (+) (fmap (a0 *) b)
        $ multiple a b'

  abs :: Num a => Series a -> Series a
  abs (Series s) = Series $ abs <$> s

  signum :: Num a => Series a -> Series a
  signum (Series s) = Series $ signum <$> s

-- | Multiplies power series by given number
-- 
-- For following series
--   @a0 + a1 * x + a2 * x^2 + ...@
-- coefficients would be
--   @a0, a1, a2, ...@
--
-- Usage examples:
--
-- >>> coefficients (2 *: (x + x ^ 2 + x ^ 4))
-- [0,2,2,0,2,0,0,0,0,0]
-- >>> coefficients (2 *: ((1 + x)^5))
-- [2,10,20,20,10,2,0,0,0,0]
--
infixl 7 *:
(*:) :: Num a => a -> Series a -> Series a
a *: Series s = Series $ (a *) <$> s

instance Fractional a => Fractional (Series a) where
  fromRational :: Fractional a => Rational -> Series a
  fromRational r = Series $ fromList 0 [fromRational r]
  
  (/) :: Fractional a => Series a -> Series a -> Series a
  Series s1 / Series s2 = Series $ divide s1 s2
    where 
      divide (Stream a0 a) b'@(Stream b0 b) = Stream (a0 / b0)
        $ divide
          (zipWithStream (-) a
            (fmap ((a0 / b0) *) b)
          )
          b'

-- | Helper function for producing integer
-- coefficients from generating function
-- (assuming denominator of 1 in all coefficients)
--
-- Usage example:
--
-- >>> gen $ (2 + 3 * x)
-- [2,3,0,0,0,0,0,0,0,0]
--
gen :: Series (Ratio Integer) -> Stream Integer
gen = fmap numerator . coefficients

-- | Returns infinite stream of ones
--
-- First 10 elements:
--
-- >>> ones
-- [1,1,1,1,1,1,1,1,1,1]
--
ones :: Stream Integer
ones = gen (1 / (1 - x))

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = gen (1 / ((1 - x) * (1 - x)))

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = gen (x / (1 - x - x * x))
