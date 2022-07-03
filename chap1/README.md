# Chapter 1

Chapter 1 contains description of what is AST and [provides](https://www.cs.princeton.edu/~appel/modern/ml/chap1/slp.sml) us with this exmple in Standart ML:

```sml
type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))
```

The book describes two functions to implement:
- `maxargs : stm -> int` which was reimplemented pretty trivially
- `interp : stm -> unit` which was supposed to have sideffects, and relied on two others mutually recursive
  - `interpStm : stm * table -> table` as for `AssignStm` it would update the table of variable definitions
  - `inteprExp : stm * table -> int * table` which could both evaluate to some value, as well as modify state

[`src/Lib.hs`](src/Lib.hs) contains Haskell reimplementaion of AST definition, as well as those two functions. Instead of passing table around, I decided to use StateT, with IO monad for interpreter
