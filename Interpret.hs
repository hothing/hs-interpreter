module Interpret (
    Scalar,
    Context,
    createContext,
    getValue,
    isError,
    eval
  ) where
import Synt
import Data.Map as M

data Scalar = SInt Int | SReal Double
  deriving (Eq, Ord, Show)

data Context = Context {
    variables :: M.Map String Scalar,
    isError ::  Bool -- TODO: devision by zero?
  }
  deriving (Eq, Show)

createContext :: Context
createContext = Context { variables = M.empty, isError = False }

getValue :: Context -> String -> Maybe Scalar
getValue ctx name = M.lookup name $ variables ctx

eval :: Context -> Synt.Expr -> Context
eval ctx (Expr var expr) = ctx
