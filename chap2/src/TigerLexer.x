{
{-# LANGUAGE NamedFieldPuns #-}
module TigerLexer (lexTiger, prog) where
import Tokens
}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literally_any = [.\n]  -- . is the same as [^\n]

tokens :-
<0> $white+                         ;
<0> "/*"                            { begin comment }
<comment> $literally_any            { skip }
<comment> "*/"                      { begin 0 }
<0> $digit+                         { readInteger }
<0>    \"                           { setBufferStart `andBegin` str }
<str>  \\n                          { addSpecificChar '\n' }
<str>  \\t                          { addSpecificChar '\t' }
<str>  \\\"                         { addSpecificChar '\"' }
<str>  \\\\                         { addSpecificChar '\\' }
<str>  [^\"]                        { scanStringItem }
<str>  \"                           { endStr `andBegin` 0 }
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
-- prog = "   /* some */ \"string\\\" here\" "
prog = "  /*comment lslalsdf \n \n */ */ "

readInteger ((AlexPn _ line col), _, _, val) len = pure $ TokInt (line, col) (read $ take len val)

-- readStr  ((AlexPn _ line col), _, _, val) len = pure $ TokString (line, col) (take len val)
-- pos construct (AlexPn _ line col) _ = construct (line, col)

data AlexUserState = AlexUserState { lexerStringBuffer :: String, bufferStart :: Pos }

alexInitUserState = AlexUserState { lexerStringBuffer = "", bufferStart = undefined }

get :: Alex AlexState
get = Alex $ \st -> Right (st, st)

setBufferStart ((AlexPn _ line col), _, _, _) _ = do
  st <- alexGetUserState
  alexSetUserState $ st { bufferStart = (line, col)}
  alexMonadScan


addSpecificChar c _ _ = do
  st@(AlexUserState { lexerStringBuffer }) <- alexGetUserState
  alexSetUserState $ st {lexerStringBuffer = (c:lexerStringBuffer )}
  alexMonadScan

scanStringItem (_, _, _, (c:_)) _ = do
  st@(AlexUserState { lexerStringBuffer }) <- alexGetUserState
  alexSetUserState $ st {lexerStringBuffer = (c:lexerStringBuffer )}
  alexMonadScan

endStr _ _ = do
  AlexUserState { lexerStringBuffer, bufferStart } <- alexGetUserState
  alexSetUserState alexInitUserState
  (pure $ TokString bufferStart (reverse lexerStringBuffer)) 

alexEOF :: Alex Token
alexEOF = do
  (AlexState {alex_pos = (AlexPn _ line col), alex_scd = code }) <- get
  case code of
    0 -> pure TokEOF
    x | x == comment -> alexError $ "Unclosed comment. Unexpected EOF at " <> show line <> ":" <> show col
    --   | x == str -> alexError $ "Unclosed string. " <> show line <> ":" <> show col
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