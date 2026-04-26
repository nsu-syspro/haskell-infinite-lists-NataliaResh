{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task2 where

-- | Infinite stream of elements
data Stream a = Stream a (Stream a)

instance Foldable Stream where
  foldMap :: Monoid m => (a -> m) -> Stream a -> m
  foldMap f (Stream x s) = f x <> foldMap f s

instance Show a => Show (Stream a) where
  show :: Stream a -> String
  show s = "[" ++ doShow 10 s ++ "]"
    where
      doShow :: Int -> Stream a -> String
      doShow n (Stream x st)
        | n <= 0 = ""
        | n == 1 = show x
        | otherwise = show x ++ "," ++ doShow (n - 1) st

-- | Converts given list into stream
--
-- If the list is finite then it is continued
-- with given value repeated infinitely
--
-- Usage example:
--
-- >>> fromList 0 [1,2,3]
-- [1,2,3,0,0,0,0,0,0,0]
-- >>> fromList undefined [1..]
-- [1,2,3,4,5,6,7,8,9,10]
--
fromList :: a -> [a] -> Stream a
fromList y = foldr Stream (Stream y (fromList y []))

-- | Builds stream from given seed value by applying given step function
--
-- Step function produces a pair of the next element in stream and updated seed value.
--
-- Usage example:
--
-- >>> unfold (\x -> (x, x-1)) 5
-- [5,4,3,2,1,0,-1,-2,-3,-4]
-- >>> unfold (\x -> (abs x, x-1)) 5
-- [5,4,3,2,1,0,1,2,3,4]
--
unfold :: (b -> (a, b)) -> b -> Stream a
unfold f x = 
  let
    (a, b) = f x
  in Stream a (unfold f b)

-- | Returns infinite stream of natural numbers (excluding zero)
--
-- First 10 natural numbers:
--
-- >>> nats
-- [1,2,3,4,5,6,7,8,9,10]
--
nats :: Stream Integer
nats = unfold (\x -> (x, x + 1)) 1

-- | Returns infinite stream of fibonacci numbers (starting with zero)
--
-- First 10 fibonacci numbers:
--
-- >>> fibs
-- [0,1,1,2,3,5,8,13,21,34]
--
fibs :: Stream Integer
fibs = unfold (\(a, b) -> (a, (b, a + b))) (0, 1)

-- | Returns infinite stream of prime numbers
--
-- First 10 prime numbers:
--
-- >>> primes
-- [2,3,5,7,11,13,17,19,23,29]
--
primes :: Stream Integer
primes = unfold sieve $ fromList 0 [2..]

-- | One step of Sieve of Eratosthenes
-- (to be used with 'unfoldr')
--
-- Returns next prime number from given stream
-- and strikes out all multiples of this prime
-- from the rest of the stream
--
-- Usage example:
--
-- >>> sieve $ fromList 0 [2..]
-- (2,[3,5,7,9,11,13,15,17,19,21])
-- >>> sieve $ snd $ sieve $ fromList 0 [2..]
-- (3,[5,7,11,13,17,19,23,25,29,31])
--
sieve :: Stream Integer -> (Integer, Stream Integer)
sieve (Stream x s) = (x, filterStream ((/= 0) . (`mod` x)) s)
  where
    filterStream p (Stream y st)
      | p y = Stream y (filterStream p st)
      | otherwise = filterStream p st
