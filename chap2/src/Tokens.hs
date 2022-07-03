module Tokens where

type Pos = (Int, Int)

data Token
  = TokType Pos
  | TokVar Pos
  | TokFunction Pos
  | TokBreak Pos
  | TokOf Pos
  | TokEnd Pos
  | TokIn Pos
  | TokNil Pos
  | TokLet Pos
  | TokDo Pos
  | TokTo Pos
  | TokFor Pos
  | TokWhile Pos
  | TokElse Pos
  | TokThen Pos
  | TokIf Pos
  | TokArray Pos
  | TokAssign Pos
  | TokOr Pos
  | TokAnd Pos
  | TokGe Pos
  | TokGt Pos
  | TokLe Pos
  | TokLt Pos
  | TokNeq Pos
  | TokEq Pos
  | TokDivide Pos
  | TokTimes Pos
  | TokMinus Pos
  | TokPlus Pos
  | TokDot Pos
  | TokRbrace Pos
  | TokLbrace Pos
  | TokRbrack Pos
  | TokLbrack Pos
  | TokRparen Pos
  | TokLparen Pos
  | TokSemicolon Pos
  | TokColon Pos
  | TokComma Pos
  | TokString Pos String -- TODO: Implement Lexer rule
  | TokInt Pos Int -- TODO: Implement Lexer rule
  | TokId Pos String -- TODO: Implement Lexer rule
  deriving (Show)

-- | TokEof Pos