{
module Lexer (lexTiger, prog) where
import Tokens
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                        ;
  "/*"[.\n]*"*/"                 ;
  $digit+                        { \(AlexPn _ line col) val -> TokInt (line, col) (read val) }
  type                           { pos TokType }
  var                            { pos TokVar }
  function                       { pos TokFunction }
  break                          { pos TokBreak }
  of                             { pos TokOf }
  end                            { pos TokEnd }
  in                             { pos TokIn }
  nil                            { pos TokNil }
  let                            { pos TokLet }
  do                             { pos TokDo }
  to                             { pos TokTo }
  for                            { pos TokFor }
  while                          { pos TokWhile }
  else                           { pos TokElse }
  then                           { pos TokThen }
  if                             { pos TokIf }
  array                          { pos TokArray }
  "="                            { pos TokAssign }
  "||"                           { pos TokOr }
  "&&"                           { pos TokAnd }
  ">="                           { pos TokGe }
  ">"                            { pos TokGt }
  "<="                           { pos TokLe }
  "<"                            { pos TokLt }
  "!="                           { pos TokNeq }
  "=="                           { pos TokEq }
  "/"                            { pos TokDivide }
  "*"                            { pos TokTimes }
  "-"                            { pos TokMinus }
  "+"                            { pos TokPlus }
  "."                            { pos TokDot }
  "}"                            { pos TokRbrace }
  "{"                            { pos TokLbrace }
  "["                            { pos TokRbrack }
  "]"                            { pos TokLbrack }
  ")"                            { pos TokRparen }
  "("                            { pos TokLparen }
  ";"                            { pos TokSemicolon }
  ":"                            { pos TokColon }
  ","                            { pos TokComma }
{
-- Each action has type :: AlexPosn -> String -> Token
prog = "123 /* some */"
pos construct (AlexPn _ line col) _ = construct (line, col)
lexTiger = alexScanTokens
}