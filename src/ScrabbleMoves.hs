{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleMoves where

import qualified Data.Map as Map
import Data.List
import Data.Ord
import Data.Array
import Data.Maybe
import ScrabbleTypes
import ScrabbleDict
import ScrabbleInit
import ScrabbleMisc
import Data.Either

isOnBoard board p = isJust $ cboard !!! p
	where cboard = charBoard board

center board = let (i,j) = snd $ bounds board 
	       in (uncurry (+) (divMod i 2),uncurry (+) (divMod j 2))

splitMove :: Board -> Move -> SplitMove
splitMove board move = SplitMove { onBoard = on, offBoard = off} 
	where unsplitMove = zip (moveRange move) (getWord move)
	      (on,off) = partition (isOnBoard board . fst) unsplitMove
	      
moveRange :: Move -> [(Int,Int)]
moveRange (Move (i,j) dir word) = case dir of
    Horizontal -> [(i,j+k) | k <- [0..len-1]]
    Vertical   -> [(i+k,j) | k <- [0..len-1]]
  where len = length word

consistent :: Board -> SplitMove -> Bool
consistent board (SplitMove on _) = and $ zipWith onAndEqual (map ((cboard !!!) . fst) on) (map snd on) 
	where onAndEqual Nothing _ = False
	      onAndEqual (Just x) y = x == y
	      cboard = charBoard board

scoreMove :: Board -> SplitMove -> Double
scoreMove board (SplitMove on off) = wordmod*(sum (map (scoreLetter' . snd) on) + sum (map applyLetterMod lettermodded))
	where modded :: [(Char,Modifier)]
	      modded = zip (map snd off) (map ((mboard !) . fst) off)
	      (wordmodded,lettermodded) = partition (isWordMod . snd) modded
	      wordmod = product $ map (getMod . snd) wordmodded
	      applyLetterMod (c,LetterMod m) = m*scoreLetter' c
	      mboard = modBoard board

makeMove :: Board -> Move -> Either String Board
makeMove board move | firstMove board && not intersectCenter = Left "The first move must intersect the center point, which is labeled with a star."
		    | not $ all (withinBounds cboard) (moveRange move) = Left "Your move attempts to go outside the board."
		    | not (consistent board smove) = Left "Your move is not consistent with the tiles already on the board."
		    | not $ subset (map snd off) (playerRack $ curr_player board) = Left "tiles not available."
		    | not $ getWord move `Map.member` dictionaryMap = Left "word not in dictionary."
		    | otherwise = Right (updateBoard board smove)
	where smove@(SplitMove _ off) = splitMove board move
	      intersectCenter = any (== center cboard) (moveRange move)
	      cboard = charBoard board
--TODO: re-introduce code for too many adjacencies.

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys = x `elem` ys && subset xs (delete x ys) 

updateBoard :: Board -> SplitMove -> Board
updateBoard board move@(SplitMove _ off) = board { charBoard = charBoard board // map (fmap Just) off,
	       			                   player_ind = mod (i+1) (n_players board),
	       			                   players = (players board) // [(i,new_player_i)],
	       			                   tile_seq = transferred_seq,
				                   firstMove = False }
  	where i = player_ind board
	      old_player_i = players board ! i
	      (transferred_seq,new_rack) = transfer (length off) (tile_seq board) (playerRack old_player_i \\ map snd off)
	      new_player_i = Player {playerRack = new_rack, playerScore = playerScore old_player_i + scoreMove board move} 
