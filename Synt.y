{
module Main where
import Lex 
}

%name synt
%tokentype { Token }
%error { parseError }
%token
  int					{ TInt $$ }
  real					{ TReal $$ }
  binop 				{ TBinOp $$ }
  unop 					{ TUnOp $$ }
  modif 				{ TModif $$ }
  "?"					{ TQuestion }
  ":"					{ TColon }
  ";" 					{ TSemiColon }
  "("					{ TLeftParen }
  ")" 					{ TRightParen }
  ident 				{ TIdent $$ }
%%

Program :
	ExprList			{ Program $1 }

ExprList :
	Expr				{ ExprList $1 ExprEnd }
	| Expr ExprList			{ ExprList $1 $2 }
	|				{ ExprEnd }

Expr :
	ident modif RValue ";"		{ Expr $1 $2 $3 }

RValue :
	RValue binop RValue		{ BinOp $1 $2 $3 }
	| unop RValue			{ UnOp $1 $2 }
	| RValue "?" RValue ":" RValue  { IfElse $1 $3 $5 }
	| "(" RValue ")"		{ Parens $2 }
	| int				{ IntVal $1 }
	| real				{ RealVal $1 }
	| ident				{ IdentVal $1 }
{
parseError :: [Token] -> a
parseError xs = error $ "Parse error near: " ++ show xs

data Program = Program ExprList
  deriving (Show, Eq)

data ExprList = ExprList Expr ExprList | ExprEnd
  deriving (Show, Eq)

data Expr = Expr String String RValue
  deriving (Show, Eq)

data RValue =	IntVal Int | RealVal Double | IdentVal String
		| BinOp RValue String RValue | UnOp String RValue | IfElse RValue RValue RValue
		| Parens RValue
  deriving (Show, Eq)

main = do 
  inStr <- getContents
  let parseTree = synt.alexScanTokens $ inStr  
  putStrLn.show $ parseTree
}
