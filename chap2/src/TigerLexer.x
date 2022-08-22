{
{-# LANGUAGE NamedFieldPuns #-}
module TigerLexer (lexTiger, prog) where
import Tokens
}

%wrapper "monadUserState"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters
$literally_any = [.\n]  -- . is the same as [^\n], so we need 'em both

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
<str> \n                            { stringNewlineError }
<str>  [^\"]                        { scanStringItem }
<str>  \"                           { endStr `andBegin` 0 }
<0> type                            { makeToken TokType }
<0> var                             { makeToken TokVar }
<0> function                        { makeToken TokFunction }
<0> break                           { makeToken TokBreak }
<0> of                              { makeToken TokOf }
<0> end                             { makeToken TokEnd }
<0> in                              { makeToken TokIn }
<0> nil                             { makeToken TokNil }
<0> let                             { makeToken TokLet }
<0> do                              { makeToken TokDo }
<0> to                              { makeToken TokTo }
<0> for                             { makeToken TokFor }
<0> while                           { makeToken TokWhile }
<0> else                            { makeToken TokElse }
<0> then                            { makeToken TokThen }
<0> if                              { makeToken TokIf }
<0> array                           { makeToken TokArray }
<0> $alpha+($digit|$alpha|"_")*     { readId }
<0> ":="                            { makeToken TokAssign }
<0> "||"                            { makeToken TokOr }
<0> "&&"                            { makeToken TokAnd }
<0> ">="                            { makeToken TokGe }
<0> ">"                             { makeToken TokGt }
<0> "<="                            { makeToken TokLe }
<0> "<"                             { makeToken TokLt }
<0> "<>"                            { makeToken TokNeq }
<0> "="                             { makeToken TokEq }
<0> "/"                             { makeToken TokDivide }
<0> "*"                             { makeToken TokTimes }
<0> "-"                             { makeToken TokMinus }
<0> "+"                             { makeToken TokPlus }
<0> "."                             { makeToken TokDot }
<0> "}"                             { makeToken TokRbrace }
<0> "{"                             { makeToken TokLbrace }
<0> "["                             { makeToken TokRbrack }
<0> "]"                             { makeToken TokLbrack }
<0> ")"                             { makeToken TokRparen }
<0> "("                             { makeToken TokLparen }
<0> ";"                             { makeToken TokSemicolon }
<0> ":"                             { makeToken TokColon }
<0> ","                             { makeToken TokComma }
{
-- prog = "   /* some */ \"string\\\" here\" "
prog = "  /*comment lslalsdf \n \n */ */ "

readInteger ((AlexPn _ line col), _, _, val) len = pure $ TokInt (line, col) (read $ take len val)

readId ((AlexPn _ line col), _, _, s) len = pure $ TokId (line, col) (take len s)

type Action = AlexInput -> Int -> Alex Token

makeToken :: (Pos -> Token) -> Action
makeToken token ((AlexPn _ line col), _, _, _) _  = pure $ token (line, col)

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

stringNewlineError _ _ = do
  (AlexState {alex_pos = (AlexPn _ endL endC), alex_ust = (AlexUserState { bufferStart = (startL, startC) }) }) <- get
  alexError $ "Unexpected newline in string(" <> show startL <> ":" <> show startC <> ") at (" <> show endL <> ":" <> show endC <> ")"

alexEOF :: Alex Token
alexEOF = do
  (AlexState {alex_pos = (AlexPn _ line col), alex_scd = code }) <- get
  case code of
    0 -> pure TokEOF
    x | x == comment -> alexError $ "Unclosed comment. Unexpected EOF at (" <> show line <> ":" <> show col <> ")"
      | x == str -> alexError $ "Unclosed string. (" <> show line <> ":" <> show col <> ")"
    -- x | x == str -> alexError $ "Unclosed string. Unexpected EOF at " <> show line <> ":" <> show col
    _ -> alexError $ "Unexpected EOF at (" <> show line <> ":" <> show col <> ")"

lexTiger s = runAlex s loop
  where
    loop = do
      val <- alexMonadScan
      code <- alexGetStartCode
      case val of
        TokEOF -> pure []
        _ -> (val : ) <$> loop

}