module Interpret (
    Context,
    createContext,
    getValue,
    evalProg,
    evalProgCtx
  ) where

import Synt
import Data.Map as M
import Data.Bits

type Context = Map String Int

createContext :: Context
createContext = empty

getValue :: Context -> String -> Maybe Int
getValue ctx name = M.lookup name ctx

evalProg :: Program -> Either String Context
evalProg prog = evalProgCtx createContext prog

evalProgCtx :: Context -> Program -> Either String Context
evalProgCtx ctx (Program lst) = evalProg' ctx lst
  where
    evalProg' ctx (ExprList x xs) = 
      case evalExpr ctx x of
        Right rslt -> evalProg' rslt xs
        err -> err
    evalProg' ctx ExprEnd = Right ctx

evalExpr :: Context -> Expr -> Either String Context
evalExpr ctx (Expr vname rval) =
  case evalRVal ctx rval of
    Right value -> Right $ insert vname value ctx
    Left err -> Left err

evalRVal :: Context -> RVal -> Either String Int

evalRVal ctx (IdentVal ident) = 
  case getValue ctx ident of
    Just rval -> Right rval
    Nothing -> Left $ "undefined variable '" ++ ident ++ "'"

evalRVal _ (IfElse (IntVal x) (IntVal y) (IntVal z)) = 
  Right $ if x /= 0 then y else z

evalRVal ctx (IfElse a b c) = 
  let f = evalRVal ctx in
    case f a >>= \x -> f b >>= \y -> f c >>= \z -> return (x,y,z) of
      Right (x,y,z) -> Right $ if x /= 0 then y else z
      Left err -> Left err

evalRVal _ (BinOp cmd (IntVal x) (IntVal y)) =
  evalBinOp cmd x y

evalRVal ctx (BinOp cmd a b) = 
  let f = evalRVal ctx in
    case f a >>= \x -> f b >>= \y -> return (x,y) of
      Right (x,y) -> evalBinOp cmd x y
      Left err -> Left err

evalRVal _ (UnOp cmd (IntVal x)) =
  evalUnOp cmd x

evalRVal ctx (UnOp cmd a) = 
  case evalRVal ctx a of
    Right x -> evalUnOp cmd x
    err -> err

evalRVal _ (IntVal x) = Right x

evalUnOp :: UnOpType -> Int -> Either String Int
evalUnOp cmd x =
  case cmd of
    BinNot -> Right $ complement x
    LogNot -> Right $ if not (x /= 0) then 1 else 0
    Neg -> Right (-x)

evalBinOp :: BinOpType -> Int -> Int -> Either String Int
evalBinOp cmd x y =
  case cmd of
    Add -> Right $ x + y
    Sub -> Right $ x - y
    Mul -> Right $ x * y
    BinAnd -> Right $ x .&. y
    BinOr -> Right $ x .|. y
    BinXor -> Right $ x `xor` y
    LogAnd -> Right $ if (x /= 0) && (y /= 0) then 1 else 0
    LogOr -> Right $ if (x /= 0) || (y /= 0) then 1 else 0
    LogXor -> Right $ if ((x /= 0) && (y == 0)) || ((x == 0) && (y /= 0)) then 1 else 0
    Eq -> Right $ if x == y then 1 else 0
    Le -> Right $ if x <= y then 1 else 0
    Lt -> Right $ if x < y then 1 else 0
    Shl -> Right $ x `shift` y
    Rol -> Right $ x `rotate` y
    Div -> if y == 0
             then Left "divide by zero"
             else Right $ x `div` y
    Mod -> if y == 0
             then Left "devide by zero"
             else Right $ x `mod` y

