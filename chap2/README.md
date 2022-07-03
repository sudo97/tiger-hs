# chap2 Lexers

This chapter introduces lexers. The assignment is to complete the following file called `tiger.lex`:
```sml
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
","	=> (Tokens.COMMA(yypos,yypos+1));
var  	=> (Tokens.VAR(yypos,yypos+3));
"123"	=> (Tokens.INT(123,yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

```

There are also `tokens.sig`
```sml
signature Tiger_TOKENS =
sig
type linenum (* = int *)
type token
val TYPE:  linenum * linenum -> token
val VAR:  linenum * linenum -> token
val FUNCTION:  linenum * linenum -> token
val BREAK:  linenum * linenum -> token
val OF:  linenum * linenum -> token
val END:  linenum * linenum -> token
val IN:  linenum * linenum -> token
val NIL:  linenum * linenum -> token
val LET:  linenum * linenum -> token
val DO:  linenum * linenum -> token
val TO:  linenum * linenum -> token
val FOR:  linenum * linenum -> token
val WHILE:  linenum * linenum -> token
val ELSE:  linenum * linenum -> token
val THEN:  linenum * linenum -> token
val IF:  linenum * linenum -> token
val ARRAY:  linenum * linenum -> token
val ASSIGN:  linenum * linenum -> token
val OR:  linenum * linenum -> token
val AND:  linenum * linenum -> token
val GE:  linenum * linenum -> token
val GT:  linenum * linenum -> token
val LE:  linenum * linenum -> token
val LT:  linenum * linenum -> token
val NEQ:  linenum * linenum -> token
val EQ:  linenum * linenum -> token
val DIVIDE:  linenum * linenum -> token
val TIMES:  linenum * linenum -> token
val MINUS:  linenum * linenum -> token
val PLUS:  linenum * linenum -> token
val DOT:  linenum * linenum -> token
val RBRACE:  linenum * linenum -> token
val LBRACE:  linenum * linenum -> token
val RBRACK:  linenum * linenum -> token
val LBRACK:  linenum * linenum -> token
val RPAREN:  linenum * linenum -> token
val LPAREN:  linenum * linenum -> token
val SEMICOLON:  linenum * linenum -> token
val COLON:  linenum * linenum -> token
val COMMA:  linenum * linenum -> token
val STRING: (string) *  linenum * linenum -> token
val INT: (int) *  linenum * linenum -> token
val ID: (string) *  linenum * linenum -> token
val EOF:  linenum * linenum -> token
end
```

Which was followed by this implementation `tokens.sml`:
```sml
structure Tokens : Tiger_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

type linenum = int
type token = string
fun TYPE(i,j) = "TYPE   " ^ Int.toString(i)
fun VAR(i,j) = "VAR   " ^ Int.toString(i)
fun FUNCTION(i,j) = "FUNCTION   " ^ Int.toString(i)
fun BREAK(i,j) = "BREAK   " ^ Int.toString(i)
fun OF(i,j) = "OF   " ^ Int.toString(i)
fun END(i,j) = "END   " ^ Int.toString(i)
fun IN(i,j) = "IN   " ^ Int.toString(i)
fun NIL(i,j) = "NIL   " ^ Int.toString(i)
fun LET(i,j) = "LET   " ^ Int.toString(i)
fun DO(i,j) = "DO   " ^ Int.toString(i)
fun TO(i,j) = "TO   " ^ Int.toString(i)
fun FOR(i,j) = "FOR   " ^ Int.toString(i)
fun WHILE(i,j) = "WHILE   " ^ Int.toString(i)
fun ELSE(i,j) = "ELSE   " ^ Int.toString(i)
fun THEN(i,j) = "THEN   " ^ Int.toString(i)
fun IF(i,j) = "IF   " ^ Int.toString(i)
fun ARRAY(i,j) = "ARRAY   " ^ Int.toString(i)
fun ASSIGN(i,j) = "ASSIGN   " ^ Int.toString(i)
fun OR(i,j) = "OR   " ^ Int.toString(i)
fun AND(i,j) = "AND   " ^ Int.toString(i)
fun GE(i,j) = "GE   " ^ Int.toString(i)
fun GT(i,j) = "GT   " ^ Int.toString(i)
fun LE(i,j) = "LE   " ^ Int.toString(i)
fun LT(i,j) = "LT   " ^ Int.toString(i)
fun NEQ(i,j) = "NEQ   " ^ Int.toString(i)
fun EQ(i,j) = "EQ   " ^ Int.toString(i)
fun DIVIDE(i,j) = "DIVIDE   " ^ Int.toString(i)
fun TIMES(i,j) = "TIMES   " ^ Int.toString(i)
fun MINUS(i,j) = "MINUS   " ^ Int.toString(i)
fun PLUS(i,j) = "PLUS   " ^ Int.toString(i)
fun DOT(i,j) = "DOT   " ^ Int.toString(i)
fun RBRACE(i,j) = "RBRACE   " ^ Int.toString(i)
fun LBRACE(i,j) = "LBRACE   " ^ Int.toString(i)
fun RBRACK(i,j) = "RBRACK   " ^ Int.toString(i)
fun LBRACK(i,j) = "LBRACK   " ^ Int.toString(i)
fun RPAREN(i,j) = "RPAREN   " ^ Int.toString(i)
fun LPAREN(i,j) = "LPAREN   " ^ Int.toString(i)
fun SEMICOLON(i,j) = "SEMICOLON   " ^ Int.toString(i)
fun COLON(i,j) = "COLON   " ^ Int.toString(i)
fun COMMA(i,j) = "COMMA   " ^ Int.toString(i)
fun STRING(s,i,j) = "STRING("^s^")     " ^ Int.toString(i)
fun INT(c,i,j) = "INT("^Int.toString(c)^")   " ^ Int.toString(i)
fun ID(s,i,j) = "ID("^s^")     " ^ Int.toString(i)
fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
```

`errorms.sml`:
```sml
signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
end

structure ErrorMsg : ERRORMSG =
struct

  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn

  fun reset() = (anyErrors:=false;
		 fileName:="";
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)

  exception Error

  fun error pos (msg:string) =
      let fun look(a::rest,n) =
		if a<pos then app print [":",
				       Int.toString n,
				       ".",
				       Int.toString (pos-a)]
		       else look(rest,n-1)
	    | look _ = print "0.0"
       in anyErrors := true;
	  print (!fileName);
	  look(!linePos,!lineNum);
	  print ":";
	  print msg;
	  print "\n"
      end

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

end  (* structure ErrorMsg *)
```

`driver.sml`:
```sml
structure Parse =
struct 
  fun parse filename =
      let val file = TextIO.openIn filename
	  fun get _ = TextIO.input file
	  val lexer = Mlex.makeLexer get
	  fun do_it() =
	      let val t = lexer()
	       in print t; print "\n";
		   if substring(t,0,3)="EOF" then () else do_it()
	      end
       in do_it();
	  TextIO.closeIn file
      end

end
```

We gonna use Haskell, and [Alexa](https://haskell-alex.readthedocs.io/en/latest/introduction.html) for doing that