{-# LANGUAGE NoMonomorphismRestriction #-}
module ScrabbleIO where

import ScrabbleTypes
import Data.Array
import ScrabbleDict
import ScrabbleInit
import Data.List
import Data.Ord
import ScrabbleMoves
import ScrabbleMisc
import Data.Maybe
import Data.Either
import ScrabbleShow
import Data.Char

getMove :: IO Move
getMove = errCycle' $ fmap (parseMove . words) getLine 
	where parseMove :: [String] -> Either String Move
	      parseMove [startstr,dirstr,word] = case (reads startstr,dirstr) of
		([(ind,[])],"V") -> Right $ Move ind Vertical word
		([(ind,[])],"H") -> Right $ Move ind Horizontal word
		_ -> retry
	      parseMove _ = retry
	      retry = Left "Move not formatted correctly."
getConfig :: IO BoardConf
getConfig = do  putStrLn "Would you like a tutorial y/n?" 
		askCycle (return ()) tutorial
		putStrLn "Tile fraction at end of game? The official game's value is about 0.45."
		tf <- fracget	
		putStrLn "Fraction of tiles which are vowels? The official game's value is about 0.44; the gismu list's value is 0.4."
		vf <- fracget
		putStrLn "Board size?"
		bs <- intget
		putStrLn "Using unmodified board"
		putStrLn "Number of players?"
		pc <- intget
		putStrLn "Number of tiles per player?"
		tc <- intget
		return Conf { tile_frac = tf,
				vowel_frac = vf,
				modArray = emptyModArray bs,
				player_count = pc,
				player_tiles = tc
				}
doubleget :: IO Double 
doubleget = errCycle' $ fmap (doubleCheck . reads) getLine	
	where doubleCheck [(d,[])] = Right d
	      doubleCheck _ = Left "You must input a floating point number."
fracget :: IO Double
fracget = errCycle' $ fmap fracCheck doubleget
	where fracCheck d | d >= 0 && d <= 1 = Right d
			  | otherwise = Left "You must input a number between 0 and 1."
intget :: IO Int
intget = errCycle' $ fmap (intCheck . reads) getLine
	where intCheck [(i,[])] = Right i
	      intCheck _ = Left "You must input an integer."

tutorial = putStrLn "Moves are formatted as \"(4,6) V gismu\" or \"(6,4) H gismu\". The former puts \"gismu\" vertically beginning at (4,6); the latter puts \"gismu\" horizontally beginning at (6,4). On an 11x11 grid these would both pass through the middle, and hence be valid first moves. This section is still under construction."

notiles board = "There are no more tiles. " ++ tiles board

tiles board = "Player "++show (player_ind board)++", would you like to make a move y/n?"

endGame :: Board -> IO ()
endGame board = let winner_ind = maximumBy (comparing (\i -> playerScore $ players board ! i)) [0..n_players board-1] 
		in putStrLn $ "Congratulations, player "++show winner_ind++", you won!"

play :: Board -> IO ()
play board = do 
  print board
  board' <- errCycle' $ fmap (makeMove board) getMove
  putStrLn $ if (null $ tile_seq board') then notiles board' else tiles board'
  askCycle (endGame board') (play board')

askyn :: b -> b -> String -> Either String b
askyn n y yn = case map toLower yn of "n" -> Right n
				      "y" -> Right y
				      _   -> Left "y or n is required."
askCycle n y = errCycle id $ fmap (askyn n y) getLine
