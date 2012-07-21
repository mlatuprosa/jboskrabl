{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleMisc where

import Data.Array
import Data.List
infixl 9 !!!

transfer :: Integral a => a -> [b] -> [b] -> ([b],[b])
transfer n xs ys = (genericDrop n xs,genericTake n xs ++ ys)

(!!!) :: (Ix i,Ord i) => Array i (Maybe a) -> i -> Maybe a
--safe version of ! for Maybe arrays; used to deal with the boundary
--in ScrabbleMoves
arr !!! i | withinBounds arr i = arr ! i
	  | otherwise = Nothing
withinBounds arr i = i >= minInd && i <= maxInd
	where (minInd,maxInd) = bounds arr

lazyLenGreater :: (Ord a,Num a) => a -> [b] -> Bool
lazyLenGreater x zs = helper zs 0
--semantically, lazyLenGreater x zs <-> x > genericLength zs;
--but this implementation terminates in the > case without needing to evaluate all of zs
--Given that a tile sequence may be hundreds or thousands of tiles long while m*n is normally a few dozen,
--this is a significant optimization
	where helper []     acc = x > acc
	      helper (y:ys) acc = x > acc && helper ys (acc+1)

lazyLenEq :: (Ord a,Num a) => a -> [b] -> Bool
--Cf. lazyLenGreater; analogous to x == genericLength zs
lazyLenEq x zs = helper zs 0
  where helper [] acc = x == acc
	helper (y:ys) acc | x == acc = True
			  | x <  acc = False
			  | x >  acc = helper ys (acc+1)

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust ((Just x):ms) = x:takeWhileJust ms
takeWhileJust _             = []

duplicates = duplicatesSorted . sort
 
duplicatesSorted (x:y:ys) = x == y || duplicatesSorted (y:ys)
duplicatesSorted _ = False
