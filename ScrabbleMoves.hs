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

makeMove :: Board -> Move -> Either String Board
makeMove board move@(Move m)
  | firstMove board && not intersectCenter = Left "The first move must intersect the center point, which is labeled with a star."
  | null $ getWord board move = Left "move has invalid form"
  | not $ getWord board move `Map.member` dictionaryMap = Left "word not in dictionary"
  | not $ subset (map snd m) (playerRack $ curr_player board) = Left "tiles not available"
  | otherwise = Right (updateBoard board move)
  where intersectCenter = center cboard `elem` inds
	inds = map fst m
	cboard = charBoard board
subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = helper xs ys xs ys
  where helper [] _ _ _ = True
        helper _ [] _ _ = False
	helper (a:as) (b:bs) cs ds | a == b = helper (delete a cs) (delete b ds) (delete a cs) (delete b ds)
				   | otherwise = helper (a:as) bs cs ds 
updateBoard :: Board -> Move -> Board
updateBoard board move@(Move m) = board { charBoard = charBoard board // map (fmap Just) m,
	       			          player_ind = mod (i+1) (n_players board),
	       			          players = (players board) // [(i,new_player_i)],
	       			          tile_seq = transferred_seq,
				          firstMove = False }
  	where i = player_ind board
	      old_player_i = players board ! i
	      (transferred_seq,new_rack) = transfer (length m) (tile_seq board) (playerRack old_player_i \\ map snd m)
	      new_player_i = Player {playerRack = new_rack, playerScore = playerScore old_player_i + scoreMove board move} 

scoreMove :: Board -> Move -> Double
scoreMove board move@(Move m) = wordmod*(sum $ (zipWith (\m s -> (m-1)*s) (map getMod letterMods) (map (scoreLetter' . snd) m)) ++ (map scoreLetter' $ getWord board move))
  where cboard = charBoard board
        mboard = modBoard  board
        (wordMods,letterMods) = partition isWordMod $ map (mboard !) (map fst m)
	wordmod = product $ map getMod wordMods
getWord :: Board -> Move -> String
getWord board move@(Move m) 
  | firstMove board = map snd (sortBy (comparing fst) m)
  --the necessary checks here are in makeMove; the onBoard and tooManyAdjacent checks are guaranteed on the first move.
  | any (\ind -> onBoard ind || tooManyAdjacent ind) inds || not straight = []
--no duplications or 2x2 or larger squares, and straightness required; once past this point we should mostly be OK 
  | vertical = let (x,max_y) = maximumBy (comparing fst) inds
		   (_,min_y) = minimumBy (comparing fst) inds
		   topinds = [(x,y) | y <- [max_y+1..]]
		   bottominds = [(x,y) | y <- [min_y-1,min_y-2..]]
		   top = zip topinds $ takeWhileJust $ map (cboard !!!) topinds
		   bottom = zip bottominds $ takeWhileJust $ map (cboard !!!) bottominds
	       in map snd $ sortBy (comparing fst) $ top++m++bottom
   | horizontal = let (max_x,y) = maximumBy (comparing snd) inds
		      (min_x,_) = minimumBy (comparing snd) inds
		      leftinds = [(x,y) | x <- [min_x-1,min_x-2..]]
		      rightinds = [(x,y) | x <- [max_x+1..]]
		      left = zip leftinds $ takeWhileJust $ map (cboard !!!) leftinds
		      right = zip rightinds $ takeWhileJust $ map (cboard !!!) rightinds
		  in map snd $ sortBy (comparing fst) $ left++m++right
    where straight = vertical || horizontal
	  vertical = isSingleton (nub $ map snd inds)
	  horizontal = isSingleton (nub $ map fst inds)
	  onBoard p = isJust $ cboard !!! p
	  tooManyAdjacent (i,j) = or [present (i+a,j) && present (i,j+b) && present (i+a,j+b) | a <- [-1,1], b <- [-1,1]]
	  present p = onBoard p || p `elem` map fst m
	  cboard = charBoard board
	  inds = map fst m
	  isSingleton [_] = True
	  isSingleton _   = False 

center board = let (i,j) = snd $ bounds board in (div i 2+mod i 2,div j 2+mod i 2)
--EXPECTED ISSUE: "two word" moves will break with this implementation. For the Lojban application, this is unimportant, if players
--are not intentionally breaking the rules.

