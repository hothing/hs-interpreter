module Interpret (
    Scalar,
    Context,
    createContext,
    getValue,
    evalProg,
    evalExpr
  ) where
import Synt
import Data.Map as M
import Control.Applicative

data Scalar = SInt Int | SReal Double
  deriving (Eq, Ord, Show)

data Context = Context {
    variables :: M.Map String Scalar
  }
  deriving (Eq, Show)

createContext :: Context
createContext = Context { variables = M.empty }

getValue :: Context -> String -> Maybe Scalar
getValue ctx name = M.lookup name $ variables ctx

evalProg :: Synt.Program -> Either String Context
evalProg (Program lst) = evalProg' createContext lst
  where
    evalProg' ctx (ExprList x xs) = 
      case evalExpr ctx x of
        Right y -> evalProg' y xs
        z -> z
    evalProg' ctx ExprEnd = Right ctx

evalExpr :: Context -> Synt.Expr -> Either String Context
evalExpr ctx (Expr vname rval) =
  case evalRVal ctx rval of
    Right value -> Right $ ctx { variables = M.insert vname value $ variables ctx }
    Left err -> Left err

scalarToRVal :: Scalar -> Synt.RVal
scalarToRVal (SInt x) = IntVal x
scalarToRVal (SReal x) = RealVal x

evalRVal :: Context -> Synt.RVal -> Either String Scalar

evalRVal ctx (IdentVal ident) = 
  case getValue ctx ident of
    Just rval -> Right rval
    Nothing -> Left $ "Undefined variable '" ++ ident ++ "'"


evalRVal ctx (Add (IntVal x) (RealVal y)) = evalRVal ctx (Add (RealVal $ fromIntegral x) (RealVal y))
evalRVal ctx (Add (RealVal x) (IntVal y)) = evalRVal ctx (Add (RealVal x) (RealVal $ fromIntegral y))
evalRVal _ (Add (IntVal x) (IntVal y)) = Right $ SInt (x + y)
evalRVal _ (Add (RealVal x) (RealVal y)) = Right $ SReal (x + y)
evalRVal ctx (Add a b) = 
  case (evalRVal ctx a, evalRVal ctx b) of
    (Right x, Right y) -> evalRVal ctx (Add (scalarToRVal x) (scalarToRVal y))
    (Right x, err) -> err
    (err, _) -> err

evalRVal _ (IntVal x) = Right $ SInt x
evalRVal _ (RealVal x) = Right $ SReal x

evalRVal ctx rval = Right $ SInt 0
