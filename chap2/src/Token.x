{
module Main where
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

<0> $white+                         ;
<0> "--".*                          ;
<0> let                             { makeToken Let }
<0> in                              { makeToken In }
<0> $digit+                         { \(_, _, _, s) len -> pure $ Int (read (take len s)) }
<0> [\=\+\-\*\/\(\)]                { \(_, _, _, s) len -> pure $ Sym (take len s) }
<0> $alpha [$alpha $digit \_ \']*   { \(_, _, _, s) len -> pure $ Var (take len s) }

{

--type AlexInput =
-- ( AlexPosn                 -- current position,
-- , Char                     -- previous char
-- , [Byte]                   -- rest of the bytes for the current char
-- , String                   -- current input string
-- ) 

-- Action :: AlexInput -> Int -> Alex result

makeToken token _ _  = return token

data Token =
	Let 		|
	In  		|
	Sym String	|
	Var String	|
	Int Int		|
  EOF
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
  let s = "let x = 13 in x + 2"
  print (scanner s)
}