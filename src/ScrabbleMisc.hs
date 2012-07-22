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

