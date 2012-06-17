{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+				;
  $digit+				{ \s -> TInt (read s) }

  "&&"					{ \s -> TBinOp s }
  "||"					{ \s -> TBinOp s }
  "<<"					{ \s -> TBinOp s }
  ">>"					{ \s -> TBinOp s }
  "**"					{ \s -> TBinOp s }
  [\+ \- \* \/ \% \| \& \^]             { \s -> TBinOp s }

  "!!"		 			{ \s -> TUnOp  s }
  [\! \~]				{ \s -> TUnOp  s }

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
