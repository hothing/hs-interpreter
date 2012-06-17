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
  $digit+				{ \s -> TInt (read s) } -- TODO: +, -
  ("0x" $hex+) 				{ \s -> TInt (fst.head.readHex.(\(_:_:xs) -> xs) $ s) }
  ("0b" $bin+)				{ \s -> TInt (foldl (\n c -> if c == '1' then n*2+1 else n*2) 0 $ (\(_:_:xs) -> xs) $ s) }

  ($digit+ "." $digit+)			{ \s -> TReal ( fst.head.readFloat $ s :: Double ) }

  "&&"					{ \s -> TBinOp s }
  "||"					{ \s -> TBinOp s }
  "<<"					{ \s -> TBinOp s }
  ">>"					{ \s -> TBinOp s }
  "**"					{ \s -> TBinOp s }
  [\+ \- \* \/ \% \| \& \^]             { \s -> TBinOp s }

  "!!"		 			{ \s -> TUnOp  s }
  [\! \~]				{ \s -> TUnOp  s } -- TODO: unary +, -

  "<="                                  { \s -> TComOp s }
  ">="                                  { \s -> TComOp s }
  "=="                                  { \s -> TComOp s }
  "!="                                  { \s -> TComOp s }
  [\< \>]                               { \s -> TComOp s }

  "="					{ \s -> TEquals }
  ";" 					{ \s -> TSemiColon }
  "("					{ \s -> TLeftParen }
  ")"					{ \s -> TRightParen }
  [\_ $alpha] [$alpha $digit \_ ]*	{ \s -> TIdent s }
{
data Token =
	TInt Int        |
	TReal Double	|
	TBinOp String   |
	TUnOp String    |
	TComOp String   |
	TEquals         |
	TSemiColon      |
	TLeftParen 	|
	TRightParen 	|
	TIdent String
	deriving (Eq,Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}
