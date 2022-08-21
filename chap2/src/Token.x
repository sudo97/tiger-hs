{
module Token where
}

%wrapper "monadUserState"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

<0>    $white+                         ;
<0>    "--".*                          ;
<0>    let                             { makeToken Let }
<0>    in                              { makeToken In }
<0>    $digit+                         { \(_, _, _, s) len -> pure $ Int (read (take len s)) }
<0>    [\=\+\-\*\/\(\)]                { \(_, _, _, s) len -> pure $ Sym (take len s) }
<0>    \"                              { begin str }
<str>  \\n                             { addSpecificChar '\n' }
<str>  \\t                             { addSpecificChar '\t' }
<str>  \\\"                            { addSpecificChar '\"' }
<str>  [^\"]                           { scanString }
<str>  \"                              { endStr `andBegin` 0 }
<0>    $alpha [$alpha $digit \_ \']*   { \(_, _, _, s) len -> pure $ Var (take len s) }

{

--type AlexInput =
-- ( AlexPosn                 -- current position,
-- , Char                     -- previous char
-- , [Byte]                   -- rest of the bytes for the current char
-- , String                   -- current input string
-- ) 

-- Action :: AlexInput -> Int -> Alex result

data AlexUserState = AlexUserState { lexerStringValue :: String }

alexInitUserState = AlexUserState ""

makeToken token _ _  = return token

addSpecificChar c _ _ = do
  str <- lexerStringValue <$> alexGetUserState
  alexSetUserState $ AlexUserState (c:str)
  alexMonadScan

scanString (_, _, _, (c:_)) _ = do
  str <- lexerStringValue <$> alexGetUserState
  alexSetUserState $ AlexUserState (c:str)
  alexMonadScan

endStr _ _ = do
  str <- lexerStringValue <$> alexGetUserState
  alexSetUserState alexInitUserState
  pure $ Str (reverse str)

data Token =
	  Let
  | In
  | Sym String
  | Var String
  | Str String
  | Int Int
  | EOF
	deriving (Eq,Show)


alexEOF = return EOF

scanner str = runAlex str loop
  where
    loop = do
      val <- alexMonadScan
      case val of
        EOF -> pure []
        _ -> (val : ) <$> loop

main = do
  let s = "let x = 13 in x + 2 \"hello \\n \\t \\t \\\" world\""
  print (scanner s)
}