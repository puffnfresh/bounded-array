{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Array.Bounded (
  BoundedArray
, (!)
, (//)
, indexBounded
, listArrayFill
, listArrayCycle
, accumArrayBounded
) where

import           Data.Array         hiding ((!), (//))
import           Data.Array.Base    (unsafeAccumArray, unsafeAt, unsafeReplace)
import           Data.Bifunctor     (first)
import           Data.List.NonEmpty (NonEmpty, cycle, toList)
import           Prelude            hiding (cycle)

newtype IntegralIndex i
  = IntegralIndex i
  deriving (Eq, Ord, Show, Bounded, Enum, Real, Integral, Num)

instance (Integral i) => Ix (IntegralIndex i) where
  range (a, b) =
    [a..b]
  index (a, _) i =
    fromIntegral i - fromIntegral a
  inRange (a, b) i =
    a <= i && i <= b

newtype BoundedArray i e
  = BoundedArray (Array (IntegralIndex i) e)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

indexBounded :: (Bounded i, Integral i) => i -> Int
indexBounded i =
  fromIntegral (i - minBound)

unboundedArray :: (Bounded i, Integral i, Ix i) => BoundedArray i e -> Array i e
unboundedArray (BoundedArray a) =
  ixmap (minBound, maxBound) IntegralIndex a

(!) :: (Bounded i, Integral i) => BoundedArray i e -> i -> e
BoundedArray a ! i =
  a `unsafeAt` indexBounded i

(//) :: (Bounded i, Integral i) => BoundedArray i e -> [(i, e)] -> BoundedArray i e
BoundedArray a // ies =
  BoundedArray . unsafeReplace a $ fmap (first indexBounded) ies

listArrayFill :: (Bounded i, Integral i) => e -> [e] -> BoundedArray i e
listArrayFill e =
  BoundedArray . listArray (minBound, maxBound) . (++ repeat e)

listArrayCycle :: (Bounded i, Integral i) => NonEmpty e -> BoundedArray i e
listArrayCycle =
  BoundedArray . listArray (minBound, maxBound) . toList . cycle

accumArrayBounded :: (Bounded i, Integral i) => (e -> a -> e) -> e -> [(i, a)] -> BoundedArray i e
accumArrayBounded f initial =
  BoundedArray . unsafeAccumArray f initial (minBound, maxBound) . fmap (first indexBounded)
