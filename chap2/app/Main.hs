module Main where

import System.Environment (getArgs)
import System.IO
import TigerLexer (lexTiger)
import Tokens

main :: IO ()
main = do
  [file] <- getArgs
  contents <- openFile file ReadMode >>= hGetContents
  putStrLn contents
  putStrLn "/*"
  print $ lexTiger contents
  putStrLn "*/"
