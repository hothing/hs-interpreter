-- Lex.x -*- mode: haskell -*-
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
  $digit+				{ \s -> TInt (read s) }
  ("0x" $hex+) 				{ \s -> TInt (fst.head.readHex.(\(_:_:xs) -> xs) $ s) }
  ("0b" $bin+)				{ \s -> TInt (foldl (\n c -> if c == '1' then n*2+1 else n*2) 0 $ (\(_:_:xs) -> xs) $ s) }

  ($digit+ "." $digit+)			{ \s -> TReal ( fst.head.readFloat $ s :: Double ) }

  [$alpha] [$alpha $digit]*		{ \s -> TIdent s }

  "+"					{ \s -> TPlus }
  "-"					{ \s -> TMinus }
  "*"					{ \s -> TMul }
  "/"					{ \s -> TDiv }
  "%"					{ \s -> TMod }
  "|"					{ \s -> TBinOr }
  "&"					{ \s -> TBinAnd }
  "^"					{ \s -> TBinXor }
  "~"					{ \s -> TBinNot }
  "!"					{ \s -> TLogNot }
  "&&"					{ \s -> TLogAnd }
  "||"					{ \s -> TLogOr }
  "<<"					{ \s -> TShl }
  ">>"					{ \s -> TShr }

  "<"					{ \s -> TLt }
  ">"					{ \s -> TGt }
  "<="                                  { \s -> TLe }
  ">="                                  { \s -> TGe }
  "=="                                  { \s -> TEq }
  "<>"                                  { \s -> TNe }

  "="					{ \s -> TModifSet }
  "+="					{ \s -> TModifPlus }
  "-="					{ \s -> TModifMinus }
  "*="					{ \s -> TModifMul }
  "/="					{ \s -> TModifDiv }
  "%="					{ \s -> TModifMod }
  "|="					{ \s -> TModifBinOr }
  "&="					{ \s -> TModifBinAnd }
  "^="					{ \s -> TModifBinXor }
  ">>="					{ \s -> TModifShr }
  "<<="					{ \s -> TModifShl }
  "||="					{ \s -> TModifLogOr }
  "&&="					{ \s -> TModifLogAnd }

  "?"					{ \s -> TQuestion }
  ":"					{ \s -> TColon }
  ";" 					{ \s -> TSemiColon }
  "("					{ \s -> TLeftParen }
  ")"					{ \s -> TRightParen }
{
data Token =
	TInt Int | TReal Double | TIdent String 
	| TPlus | TMinus | TMul | TDiv | TMod | TBinOr | TBinAnd | TBinXor | TBinNot
	| TLogNot | TLogAnd | TLogOr | TShl | TShr | TLt | TGt | TLe | TGe | TEq | TNe
	| TModifSet | TModifPlus | TModifMinus | TModifMul | TModifDiv | TModifMod
	| TModifShr | TModifShl | TModifLogOr | TModifLogAnd |  TModifBinOr | TModifBinAnd | TModifBinXor
	| TQuestion | TColon | TSemiColon | TLeftParen | TRightParen
	deriving (Eq)

instance Show Token where
  show x = case x of
    TInt i -> show i
    TReal r -> show r
    TIdent s -> s
    TModifSet -> "="
    TModifPlus -> "+="
    TModifMinus -> "-="
    TModifMul -> "*="
    TModifDiv -> "/="
    TModifMod -> "%="
    TModifBinAnd -> "&="
    TModifBinOr -> "|="
    TModifBinXor -> "^="
    TModifShl -> "<<="
    TModifShr -> ">>="
    TModifLogOr -> "||="
    TModifLogAnd -> "&&="
    TPlus -> "+"
    TMinus -> "-"
    TMul -> "*"
    TDiv -> "/"
    TMod -> "%"
    TBinOr -> "|"
    TBinAnd -> "&"
    TBinXor -> "^"
    TBinNot -> "~"
    TLogNot -> "!"
    TLogAnd -> "&&"
    TLogOr -> "||"
    TShl -> "<<"
    TShr -> ">>"
    TLt -> "<"
    TGt -> ">"
    TLe -> "<="
    TGe -> ">="
    TEq -> "=="
    TNe -> "<>"
    TQuestion -> "?"
    TColon -> ":"
    TSemiColon -> ";"
    TLeftParen -> "("
    TRightParen -> ")"

}
