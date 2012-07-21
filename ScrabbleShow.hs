module ScrabbleShow where

import ScrabbleTypes
import ScrabbleDict
import Data.Array
import Data.Maybe
import ScrabbleMoves
instance Show Board where
  show board = showrows board ++ "\n" ++ showplayer board
instance Show Modifier where
  show (LetterMod m) | m == 1 = box $ replicate (length (show m) + 4) ' '
		     | otherwise =  box $ show m ++ "x L "
  show (WordMod m) | m == 1 = box $ replicate (length (show m) + 4) ' '
		   | otherwise = box $ show m ++ "x W "
box str = "|" ++ str ++ "|"
showrows :: Board -> String
showrows board = unlines $ map (showrow . rowList) [min_y..max_y]
	where ((min_y,min_x),(max_y,max_x)) = bounds cboard
	      rowList i = map (\j -> (cboard ! (i,j),mboard ! (i,j),isCenter mboard (i,j))) [min_x..max_x]
	      cboard = charBoard board
	      mboard = modBoard board
isCenter board = (== center board)
showrow = concatMap showtile
showtile (Just c,_,_)  = showletter c
showtile (_,_,True)    = box $ replicate 3 ' ' ++ "*" ++ replicate 3 ' '
showtile (_,m,_) = show m
showletter c = box $ c:(show (scoreLetter' c) ++ replicate (9-used_width) ' ')
  where used_width = 1 + length (show (scoreLetter' c))
--TODO: make showletter the Show for newtype Tile c = Tile { tile :: c }; 
--this requires adding constructors and destructors in many places however.
showplayer :: Board -> String
showplayer board = box (concatMap (\c -> c:(show (scoreLetter' c) ++ " ")) rack) ++ show score
	where ((_,min_x),(_,max_x)) = bounds cboard
	      (Player rack score) = curr_player board
	      width = 9*(max_x - min_x)
	      cboard = charBoard board
	      used_width = length (concatMap showletter rack) + length (show score) 
		        

