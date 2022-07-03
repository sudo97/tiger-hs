{
module Lexer (lex) where
import Tokens
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-

  $white+                        ;
  "--".*                         ;
  let                            { \pos s -> Let }
  in                             { \pos s -> In }
  $digit+                        { \pos s -> Int (read s) }
  [\=\+\-\*\/\(\)]               { \pos s -> Sym (head s) }
  $alpha [$alpha $digit \_ \']*  { \pos s -> Var s }

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
-- data Token
--   = Let
--   | In
--   | Sym Char
--   | Var String
--   | Int Int
--   deriving (Eq, Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}