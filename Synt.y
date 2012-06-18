-- Synt.y -*- mode: haskell -*-
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
	'+'				{ TPlus }
	'-'				{ TMinus }
	'*'				{ TMul }
	'/'				{ TDiv }
	'%'				{ TMod }
	'||'				{ TLogOr }
	'&&'				{ TLogAnd }
	'|'				{ TBinOr }
	'&'				{ TBinAnd }
	'^'				{ TBinXor }
	'=='				{ TEq }
	'<>'				{ TNe }
	'<'				{ TLt }
	'<='				{ TLe }
	'>'				{ TGt }
	'>='				{ TGe }
	'<<'				{ TShl }
	'>>'				{ TShr }
	'!'				{ TLogNot }
	'~'				{ TBinNot }
	modif				{ TModif $$ }
	'?'				{ TQuestion }
	':'				{ TColon }
	';' 				{ TSemiColon }
	'('				{ TLeftParen }
	')' 				{ TRightParen }
	ident 				{ TIdent $$ }

%right modif
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '<>'
%left '<' '<=' '>' '>='
%left '>>' '<<'
%left '+' '-'
%left '*' '/' '%'
%right '!' '~'
%%

Program:
	ExprList			{ Program $1 }

ExprList:
	Expr ExprList			{ ExprList $1 $2 }
	|				{ ExprEnd }

Expr:
	ident modif RVal ';'		{ Expr $1 $2 $3 }

RVal:
	RVal '+' RVal			{ Add $1 $3 }
	| RVal '-' RVal			{ Sub $1 $3 }
	| RVal '*' RVal			{ Mul $1 $3 }
	| RVal '/' RVal			{ Div $1 $3 }
	| RVal '%' RVal			{ Mod $1 $3 }
	| RVal '||' RVal		{ LogOr $1 $3 }
	| RVal '&&' RVal		{ LogAnd $1 $3 }
	| RVal '&' RVal			{ BinAnd $1 $3 }
	| RVal '|' RVal			{ BinOr $1 $3 }
	| RVal '^' RVal			{ BinXor $1 $3 }
	| RVal '==' RVal		{ Eq $1 $3 }
	| RVal '<>' RVal		{ Ne $1 $3 }
	| RVal '<' RVal			{ Lt $1 $3 }
	| RVal '<=' RVal		{ Le $1 $3 }
	| RVal '>' RVal			{ Gt $1 $3 }
	| RVal '>=' RVal		{ Ge $1 $3 }
	| RVal '<<' RVal		{ Shl $1 $3 }
	| RVal '>>' RVal		{ Shr $1 $3 }
	| '!' RVal			{ LogNot $2 }
	| '~' RVal			{ BinNot $2 }
	| RVal '?' RVal ':' RVal	{ IfElse $1 $3 $5 }
	| '(' RVal ')'			{ $2 }
	| int				{ IntVal $1 }
	| real				{ RealVal $1 }
	| ident				{ IdentVal $1 }
	
{
parseError :: [Token] -> a
parseError xs = 
  error $ "Syntax error near: " ++ concatMap (\x -> show x ++ " ") (take 16 xs)

data Program = Program ExprList
  deriving (Show, Eq)

data ExprList = ExprList Expr ExprList | ExprEnd
  deriving (Show, Eq)

data Expr = Expr String String RVal
  deriving (Show, Eq)

data RVal =	IntVal Int | RealVal Double | IdentVal String
		| Add RVal RVal | Sub RVal RVal | Mul RVal RVal | Div RVal RVal | Mod RVal RVal 
		| LogOr RVal RVal | LogAnd RVal RVal | BinAnd RVal RVal | BinOr RVal RVal | BinXor RVal RVal
		| Eq RVal RVal | Ne RVal RVal | Lt RVal RVal | Le RVal RVal | Gt RVal RVal | Ge RVal RVal
		| Shl RVal RVal | Shr RVal RVal | LogNot RVal | BinNot RVal | IfElse RVal RVal RVal
  deriving (Show, Eq)
}
