module Main where

import ScrabbleIO
import ScrabbleInit
import System.Random

main :: IO ()
main = do conf <- getConfig
	  gen <- getStdGen
	  handler $ initBoard conf (randomRs ('a','z') gen)
