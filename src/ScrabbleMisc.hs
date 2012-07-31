{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleMisc where

import Data.Array
import Data.List
infixl 9 !!!

transfer :: Integral a => a -> [b] -> [b] -> ([b],[b])
transfer n xs ys = (genericDrop n xs,genericTake n xs ++ ys)

--(!!!) :: (Ix i,Ord i) => Array i (Maybe a) -> i -> Maybe a
--safe version of ! for Maybe arrays; used to deal with the boundary
--in ScrabbleMoves
arr !!! i | withinBounds arr i = arr ! i
	  | otherwise = Nothing
withinBounds arr (i,j) = i >= mini && j >= minj && i <= maxi && j <= maxj 
	where ((mini,minj),(maxi,maxj)) = bounds arr

eitherCycle :: Monad m => (a -> m b) -> (c -> m d) -> m (Either a c) -> m d
eitherCycle lf rf m = either (\l -> lf l >> eitherCycle lf rf m) rf =<< m 

--Default argument forms
eitherCycle' :: Monad m => (a -> m b) -> m (Either a c) -> m c
eitherCycle' lf = eitherCycle lf return 

errCycle :: (a->IO b) -> IO (Either String a) -> IO b
errCycle = eitherCycle putStrLn 

errCycle' :: IO (Either String a) -> IO a
errCycle' = errCycle return


