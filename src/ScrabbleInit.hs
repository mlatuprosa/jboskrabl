{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleInit where

import Data.Array
import qualified Data.Map as Map
import ScrabbleTypes
import ScrabbleDict
import Data.List
import ScrabbleMisc
import Data.Maybe
tile_count :: BoardConf -> Int
tile_count conf = round $ fromIntegral m * fromIntegral n * tile_frac conf
  where (m,n) = snd $ bounds $ modArray conf

--Semantic warning: because of rounding, mapSum (tile_map conf) is only approximately 
--equal to tile_count conf. For reasonable total numbers of tiles,
--these errors are relatively small. 

vowel_count :: BoardConf -> Int
vowel_count conf = round $ fromIntegral (tile_count conf) * vowel_frac conf

consonant_count :: BoardConf -> Int
consonant_count conf = tile_count conf - vowel_count conf

mapSum :: (Num a,Ord k) => Map.Map k a -> a
mapSum = Map.fold (+) 0

initBoard :: BoardConf -> String -> Board
initBoard conf lookups =  Board {  tile_seq = initSeq (tile_map conf) lookups,
			           modBoard = modArray conf, 
			           charBoard = fmap (const Nothing) (modArray conf),
			  	   players = make_players conf lookups,
			           player_ind = 0,
			           n_players = player_count conf,
			           firstMove = True} 

emptyModArray :: Int -> Array (Int,Int) Modifier
emptyModArray size = listArray ((1,1),(size,size)) (repeat (LetterMod 1)) 

initSeq :: Map.Map Char Int -> String -> String
initSeq _ [] = []
initSeq dict (l:ls) | Map.null dict = []
		    | isJust (Map.lookup l dict) = l:initSeq dict' ls
		    | otherwise = initSeq dict ls
  where dict' = Map.update (deleteN 1) l dict
	deleteN n m | m <= n = Nothing
		    | otherwise = Just (m-n)

tile_map :: BoardConf -> Map.Map Char Int
tile_map conf = Map.union (vowel_map conf) (consonant_map conf)
vowel_map :: BoardConf -> Map.Map Char Int
vowel_map conf = Map.map (\f -> round $ (fractionOfSum vowelFreqs f) * (fromIntegral $ vowel_count conf)) vowelFreqs 
consonant_map :: BoardConf -> Map.Map Char Int
consonant_map conf = Map.map (\f -> round $ (fractionOfSum consonantFreqs f) * (fromIntegral $ consonant_count conf)) consonantFreqs

fractionOfSum :: (Num a,Fractional a,Ord k) => Map.Map k a -> a -> a 
fractionOfSum mp x = x / mapSum mp

make_players :: BoardConf -> String -> Array Int Player
make_players conf lookups = array (0,player_count conf - 1) $ zip [0..] $ myplayers (player_count conf) (player_tiles conf) (initSeq (tile_map conf) lookups) []

myplayers :: (Ord a,Integral a,Num a) => a->a->[Char]->[Player]->[Player]
myplayers m n ts ps | m <= 0 = ps
		    | otherwise = let (ts',rack) = transfer n ts []
		                  in myplayers (m-1) n ts' ((Player rack 0):ps)
 

