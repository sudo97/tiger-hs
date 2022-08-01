{
module TigerLexer (lexTiger, prog) where
import Tokens
}

%wrapper "monad"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literally_any = [.\n]  -- . is the same as [^\n]

tokens :-
<0> $white+                         ;
<0> "/*"                            { begin comment }
<comment> .                         { skip }
<comment> \n                        { skip }
<comment> "*/"                      { begin 0 }
<0> $digit+                         { readInteger }
-- <0> \"                               { begin str }
-- <str> [^\"]*\"                       { \((AlexPn _ line col), _, _, val) len -> alexSetStartCode 0 *> (pure $ TokString (line, col) (take (len-1) val)) }
--  <0> \".*\"                          { \(AlexPn _ line col) val -> TokString (line, col) (read val)}
--  <0> $alpha+($digit|$alpha|"_")*     { \(AlexPn _ line col) val -> TokId (line, col) val }
--  <0> type                            { pos TokType }
--  <0> var                             { pos TokVar }
--  <0> function                        { pos TokFunction }
--  <0> break                           { pos TokBreak }
--  <0> of                              { pos TokOf }
--  <0> end                             { pos TokEnd }
--  <0> in                              { pos TokIn }
--  <0> nil                             { pos TokNil }
--  <0> let                             { pos TokLet }
--  <0> do                              { pos TokDo }
--  <0> to                              { pos TokTo }
--  <0> for                             { pos TokFor }
--  <0> while                           { pos TokWhile }
--  <0> else                            { pos TokElse }
--  <0> then                            { pos TokThen }
--  <0> if                              { pos TokIf }
--  <0> array                           { pos TokArray }
--  <0> ":="                            { pos TokAssign }
--  <0> "||"                            { pos TokOr }
--  <0> "&&"                            { pos TokAnd }
--  <0> ">="                            { pos TokGe }
--  <0> ">"                             { pos TokGt }
--  <0> "<="                            { pos TokLe }
--  <0> "<"                             { pos TokLt }
--  <0> "<>"                            { pos TokNeq }
--  <0> "="                             { pos TokEq }
--  <0> "/"                             { pos TokDivide }
--  <0> "*"                             { pos TokTimes }
--  <0> "-"                             { pos TokMinus }
--  <0> "+"                             { pos TokPlus }
--  <0> "."                             { pos TokDot }
--  <0> "}"                             { pos TokRbrace }
--  <0> "{"                             { pos TokLbrace }
--  <0> "["                             { pos TokRbrack }
--  <0> "]"                             { pos TokLbrack }
--  <0> ")"                             { pos TokRparen }
--  <0> "("                             { pos TokLparen }
--  <0> ";"                             { pos TokSemicolon }
--  <0> ":"                             { pos TokColon }
--  <0> ","                             { pos TokComma }
{
prog = "   /* some */ "

readInteger ((AlexPn _ line col), _, _, val) len = pure $ TokInt (line, col) (read $ take len val)
-- pos construct (AlexPn _ line col) _ = construct (line, col)

get :: Alex AlexState
get = Alex $ \st -> Right (st, st)

alexEOF :: Alex Token
alexEOF = do
  (AlexState (AlexPn _ line col) _ _ _ code) <- get
  case code of
    0 -> pure TokEOF
    x | x == comment -> alexError $ "Unclosed comment. Unexpected EOF at " <> show line <> ":" <> show col
    -- x | x == str -> alexError $ "Unclosed string. Unexpected EOF at " <> show line <> ":" <> show col
    _ -> alexError $ "Unexpected EOF at " <> show line <> ":" <> show col

lexTiger s = runAlex s loop
  where
    loop = do
      val <- alexMonadScan
      code <- alexGetStartCode
      case val of
        TokEOF -> pure []
        _ -> (val : ) <$> loop

}