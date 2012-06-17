{
module Main (main) where
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

  [\+ \- \* \/ \% \| \& \^]             { \s -> TBinOp s }
  "&&"					{ \s -> TBinOp s }
  "||"					{ \s -> TBinOp s }
  "<<"					{ \s -> TBinOp s }
  ">>"					{ \s -> TBinOp s }
  "**"					{ \s -> TBinOp s }

  -- TODO: unary +, -
  [\! \~]				{ \s -> TUnOp  s }
  "!!"		 			{ \s -> TUnOp  s }

  [\< \>]                               { \s -> TComOp s }
  "<="                                  { \s -> TComOp s }
  ">="                                  { \s -> TComOp s }
  "=="                                  { \s -> TComOp s }
  "<>"                                  { \s -> TComOp s }

  [\!\~\+\-\*\/\%\|\&\^]? "="		{ \s -> TModif s }
  "!!="					{ \s -> TModif s }
  "**="					{ \s -> TModif s }
  ">>="					{ \s -> TModif s }
  "<<="					{ \s -> TModif s }
  "||="					{ \s -> TModif s }
  "&&="					{ \s -> TModif s }

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
	TComOp String   |
	TModif String   |
	TSemiColon      |
	TLeftParen 	|
	TRightParen 	|
	TIdent String
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
