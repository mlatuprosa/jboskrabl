module Main where

import ScrabbleIO
import ScrabbleInit
import System.Random

main :: IO ()
main = do conf <- getConfig
	  gen  <- getStdGen
	  play $ initBoard conf (randomRs ('a','z') gen)
