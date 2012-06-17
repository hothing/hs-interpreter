{
module Synt where
import Lex 
}

%name synt
%tokentype { Token }
%error { parseError }
%token
  int				{ TInt $$ }
  real				{ TReal $$ }
  binop 			{ TBinOp $$ }
  unop 				{ TUnOp $$ }
  comop				{ TComOp $$ }
  modif 			{ TModif $$ }
  "?"				{ TQuestion }
  ":"				{ TColon }
  ";" 				{ TSemiColon }
  "("				{ TLeftParen }
  ")" 				{ TRightParen }
  ident 			{ TIdent $$ }
%%

Program :
	ExprList		{ Program $1 }

ExprList :
	Expr			{ ExprList $1 ExprEnd }
	| Expr ExprList		{ ExprList $1 $2 }
	|			{ ExprEnd }

Expr :
	ident modif RValue ";"	{ Expr $1 $2 $3 }

RValue :
	int			{ RValue $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Program = Program ExprList
  deriving (Show, Eq)

data ExprList = ExprList Expr ExprList | ExprEnd
  deriving (Show, Eq)

data Expr = Expr String String RValue
  deriving (Show, Eq)

data RValue = RValue Int
  deriving (Show, Eq)

}
