{
module Lex where
import Numeric
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]
$bin = [0-1]

tokens :-
  $white+				;
  -- TODO: +, -
  $digit+				{ \s -> TInt (read s) }
  ("0x" $hex+) 				{ \s -> TInt (fst.head.readHex.(\(_:_:xs) -> xs) $ s) }
  ("0b" $bin+)				{ \s -> TInt (foldl (\n c -> if c == '1' then n*2+1 else n*2) 0 $ (\(_:_:xs) -> xs) $ s) }

  -- TODO: +, -
  ($digit+ "." $digit+)			{ \s -> TReal ( fst.head.readFloat $ s :: Double ) }

  [\+ \- \* \/ \% \| \& \^ \< \>]	{ \s -> TBinOp s }
  "&&"					{ \s -> TBinOp s }
  "||"					{ \s -> TBinOp s }
  "<<"					{ \s -> TBinOp s }
  ">>"					{ \s -> TBinOp s }
  "**"					{ \s -> TBinOp s }
  "<="                                  { \s -> TBinOp s }
  ">="                                  { \s -> TBinOp s }
  "=="                                  { \s -> TBinOp s }
  "<>"                                  { \s -> TBinOp s }

  -- TODO: unary +, -
  [\! \~]				{ \s -> TUnOp  s }
  "!!"		 			{ \s -> TUnOp  s }

  [\!\~\+\-\*\/\%\|\&\^]? "="		{ \s -> TModif s }
  "!!="					{ \s -> TModif s }
  "**="					{ \s -> TModif s }
  ">>="					{ \s -> TModif s }
  "<<="					{ \s -> TModif s }
  "||="					{ \s -> TModif s }
  "&&="					{ \s -> TModif s }

  "?"					{ \s -> TQuestion }
  ":"					{ \s -> TColon }
  ";" 					{ \s -> TSemiColon }
  "("					{ \s -> TLeftParen }
  ")"					{ \s -> TRightParen }
  [$alpha] [$alpha $digit]*		{ \s -> TIdent s }
{
data Token =
	TInt Int        |
	TReal Double	|
	TBinOp String   |
	TUnOp String    |
	TModif String   |
	TQuestion	|
	TColon		|
	TSemiColon      |
	TLeftParen 	|
	TRightParen 	|
	TIdent String
	deriving (Eq)

instance Show Token where
  show x = case x of
    TInt i -> show i
    TReal r -> show r
    TBinOp s -> s
    TUnOp s -> s
    TModif s -> s
    TIdent s -> s
    TQuestion -> "?"
    TColon -> ":"
    TSemiColon -> ";"
    TLeftParen -> "("
    TRightParen -> ")"

}
