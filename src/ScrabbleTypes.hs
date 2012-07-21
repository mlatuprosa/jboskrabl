
module ScrabbleTypes where

import Data.Array

data BoardConf = Conf { tile_frac :: Double, 
		        vowel_frac :: Double, 
		        modArray :: Array (Int,Int) Modifier,
		        player_count :: Int,
		        player_tiles :: Int}

data Modifier = LetterMod {getMod :: Double} | WordMod {getMod :: Double}

isWordMod (WordMod _) = True
isWordMod _           = False


data Board = Board { tile_seq :: String,
		     modBoard :: Array (Int,Int) Modifier,
		     charBoard :: Array (Int,Int) (Maybe Char),
		     players :: Array Int Player,
		     player_ind :: Int,
		     n_players :: Int,
		     firstMove :: Bool} 

data Direction = Horizontal | Vertical 

curr_player board = players board ! player_ind board

data Player = Player { playerRack :: String, playerScore :: Double }

data SplitMove = SplitMove { onBoard  :: [((Int,Int),Char)],
			     offBoard :: [((Int,Int),Char)] }

data Move = Move { getStart :: (Int,Int),
		   getDirection :: Direction,
		   getWord :: String }

