{
module TigerLexer (lexTiger, prog) where
import Tokens
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literally_any = [.\n]  -- . is the same as [^\n]

tokens :-
  $white+                         ;
  "/*"$literally_any*"*/"         ;
  $digit+                         { \(AlexPn _ line col) val -> TokInt (line, col) (read val) }
  \".*\"                          { \(AlexPn _ line col) val -> TokString (line, col) (read val)}
  $alpha+($digit|$alpha|"_")*     { \(AlexPn _ line col) val -> TokId (line, col) val }
  type                            { pos TokType }
  var                             { pos TokVar }
  function                        { pos TokFunction }
  break                           { pos TokBreak }
  of                              { pos TokOf }
  end                             { pos TokEnd }
  in                              { pos TokIn }
  nil                             { pos TokNil }
  let                             { pos TokLet }
  do                              { pos TokDo }
  to                              { pos TokTo }
  for                             { pos TokFor }
  while                           { pos TokWhile }
  else                            { pos TokElse }
  then                            { pos TokThen }
  if                              { pos TokIf }
  array                           { pos TokArray }
  ":="                            { pos TokAssign }
  "||"                            { pos TokOr }
  "&&"                            { pos TokAnd }
  ">="                            { pos TokGe }
  ">"                             { pos TokGt }
  "<="                            { pos TokLe }
  "<"                             { pos TokLt }
  "<>"                            { pos TokNeq }
  "="                             { pos TokEq }
  "/"                             { pos TokDivide }
  "*"                             { pos TokTimes }
  "-"                             { pos TokMinus }
  "+"                             { pos TokPlus }
  "."                             { pos TokDot }
  "}"                             { pos TokRbrace }
  "{"                             { pos TokLbrace }
  "["                             { pos TokRbrack }
  "]"                             { pos TokLbrack }
  ")"                             { pos TokRparen }
  "("                             { pos TokLparen }
  ";"                             { pos TokSemicolon }
  ":"                             { pos TokColon }
  ","                             { pos TokComma }
{
{-
  TODO:
  - Detect unclosed comments, and unclosed strings
-}
prog = "123 /* some */ if else"
pos construct (AlexPn _ line col) _ = construct (line, col)
lexTiger = alexScanTokens
}