{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleDict where

import qualified Data.Map as Map
import Data.List
import Data.Maybe
import GismuDictionary

buildFreqMap :: (Num a, Ord b) => [b] -> Map.Map b a
buildFreqMap = foldl' (\mp x -> Map.insertWith (+) x 1 mp)  Map.empty

dictionary :: [String]
dictionary = filter (\cs -> not $ elem ' ' cs) $ map (take 5) $ lines $ raw_dictionary

dictionaryMap :: Map.Map String (Maybe a)
dictionaryMap = Map.fromList [(w,Nothing) | w <- dictionary] 
letters :: String
letters = concat dictionary

letterFreqs :: Num a => Map.Map Char a 
letterFreqs = buildFreqMap letters

vowels = "aeiou"
isVowel c = c `elem` vowels

(vowelFreqs,consonantFreqs) = Map.partitionWithKey (\c _ -> isVowel c) letterFreqs


maxFreq :: (Ord a,Num b,Ord b) => Map.Map a b -> b
maxFreq = Map.fold max 0
maxConsonantFreq = maxFreq consonantFreqs

consonantRanges = [maxConsonantFreq,9*maxConsonantFreq/10..0]

pickRange :: (Ord a,Num b) => [a] -> a -> b
pickRange [] _ = error "empty range list"
pickRange [bottom] x | bottom <= x = 0
                      | otherwise = error "minimum is larger than input"
pickRange (upper:lower:ranges) x  | upper >= x && lower < x = 1
                                  | otherwise = 1 + pickRange (lower:ranges) x

--range_group :: (Num a,Ord b,Num c,Ord a,Num d) => [c] -> Map.Map b a -> Map.Map b d
rangeGroup ranges freqs = Map.map (pickRange ranges) freqs
--range_group = Map.map . pick_range

vowelPointMap :: Num a => Map.Map Char a
vowelPointMap = Map.fromList $ map (\c -> (c,1)) vowels --may range_group these instead in some way, if we find some vowels are hard
--consonant_point_map :: (Num a,Ord b) => Map.Map b a
consonantPointMap :: (Num a,Fractional a,Ord a,Enum a) => Map.Map Char a
consonantPointMap = rangeGroup consonantRanges consonantFreqs

scoreLetter :: (Num a,Ord k) => k -> Map.Map k a -> a 
scoreLetter l mp = fromMaybe 0 $ Map.lookup l mp
scoreLetter' :: (Num a,Fractional a,Ord a,Enum a) => Char -> a
scoreLetter' l = scoreLetter l $ Map.union vowelPointMap consonantPointMap
