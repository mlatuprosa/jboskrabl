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

getMove :: IO Move
getMove = do movewords <- fmap words getLine
	     parseMove movewords
	where parseMove :: [String] -> IO Move
	      parseMove [start,"H",word] = return $ Move (read start) Horizontal word
	      parseMove [start,"V",word] = return $ Move (read start) Vertical   word
	      parseMove _ = putStrLn "Your move was not formatted correctly." >> getMove

getConfig :: IO BoardConf
getConfig = do  putStrLn "Would you like a tutorial y/n?" 
		askyn'' tutorial
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
		return $ Conf { tile_frac = tf,
				vowel_frac = vf,
				modArray = emptyModArray bs,
				player_count = pc,
				player_tiles = tc
				}
doubleget :: IO Double 
doubleget = getLine >>= doubleCheck . myreads
	where doubleCheck (Just (d,[])) = return d
	      doubleCheck _ = putStrLn "You must input a floating point number." >> doubleget
fracget :: IO Double
fracget = doubleget >>= fracCheck
	where fracCheck d | d >= 0 && d <= 1 = return d
			  | otherwise = putStrLn "You must input a number between 0 and 1." >> fracget
intget :: IO Int
intget = getLine >>= intCheck . myreads
	where intCheck (Just (i,[])) = return i
	      intCheck _ = putStrLn "You must input an integer." >> intget
myreads = listToMaybe . reads

tutorial = putStrLn "Under construction."

endGame :: Board -> IO ()
endGame board = let winner_ind = maximumBy (comparing (\i -> playerScore $ players board ! i)) [0..n_players board-1] 
		in putStrLn $ "Congratulations, player "++show winner_ind++", you won!"

nearEnd :: Board -> IO ()
nearEnd board = do putStrLn $ "There are no more tiles. Player "++show (player_ind board)++", would you like to make a move y/n?"
		   askyn' (endGame board) (continue board)

handler :: Board -> IO () 
handler board | null $ tile_seq board = nearEnd board
	      | otherwise = print board >> continue board

continue :: Board -> IO ()
continue board = getMove >>= either printAndRetry handler . makeMove board
	where printAndRetry err = putStrLn err >> continue board

askyn :: IO () -> IO () -> IO () -> IO ()
askyn failure ioN ioY = do yn <- getLine
	                   case yn of "n" -> ioN
		                      "y" -> ioY
		                      _   -> failure

askyn' ioN ioY = askyn (putStrLn "You must say either y or n." >> askyn' ioN ioY) ioN ioY	

askyn'' = askyn' (return ())
