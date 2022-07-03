module Lib where

import Control.Monad.State

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Id = String

data Binop = Plus | Minus | Times | Div

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp Binop Exp
  | EseqExp Stm Exp

prog :: Stm
prog =
  CompoundStm
    (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
    ( CompoundStm
        ( AssignStm
            "b"
            ( EseqExp
                (PrintStm [IdExp "a", OpExp (IdExp "a") Minus (NumExp 1)])
                (OpExp (NumExp 10) Times (IdExp "a"))
            )
        )
        (PrintStm [IdExp "b"])
    )

maxargs :: Stm -> Int
maxargs (CompoundStm a b) = max (maxargs a) (maxargs b)
maxargs (AssignStm _ exp) = maxargsExp exp
  where
    maxargsExp :: Exp -> Int
    maxargsExp (EseqExp s _) = maxargs s
    maxargsExp _ = 0
maxargs (PrintStm args) = length args

interp :: Stm -> IO ()
interp stm = evalStateT (interpStm stm) []

type TigerContext out = StateT Table IO out

type Table = [(Id, Int)]

lookupId :: Id -> TigerContext (Maybe Int)
lookupId id = gets (lookup id)

addId :: Id -> Int -> TigerContext ()
addId id val = modify ((id, val) :)

tigerPrint :: Show a => [a] -> TigerContext ()
tigerPrint = liftIO . putStrLn . unwords . map show

interpStm :: Stm -> TigerContext ()
interpStm (CompoundStm st1 st2) = interpStm st1 *> interpStm st2
interpStm (AssignStm id exp) = interpExp exp >>= addId id
interpStm (PrintStm args) = traverse interpExp args >>= tigerPrint

interpExp :: Exp -> TigerContext Int
interpExp (IdExp id) = do
  val <- lookupId id
  case val of
    Nothing -> error "Unbound variable"
    Just n -> pure n
interpExp (NumExp num) = pure num
interpExp (OpExp e1 op e2) = do
  val1 <- interpExp e1
  val2 <- interpExp e2
  pure $ case op of
    Plus -> val1 + val2
    Minus -> val1 - val2
    Times -> val1 * val2
    Div -> val1 `div` val2
interpExp (EseqExp stm exp) = do
  interpStm stm
  interpExp exp