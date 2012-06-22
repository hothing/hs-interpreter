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

evalRVal :: Context -> Synt.RVal -> Either String Scalar

evalRVal ctx (IdentVal ident) = 
  case getValue ctx ident of
    Just rval -> Right rval
    Nothing -> Left $ "Undefined variable '" ++ ident ++ "'"

evalRVal ctx rval = Right $ SInt 0
