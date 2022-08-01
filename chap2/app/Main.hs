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
  let result = lexTiger contents
  print result
  case result of
    Right [TokString _ s] -> putStrLn s
    Left e -> putStrLn e
  pure ()
