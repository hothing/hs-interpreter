-- Lex.x -*- mode: haskell -*-
{
module Lex where
import Numeric
}

%wrapper "posn"

$alpha = [a-zA-Z]
$digit = [0-9]
$hex = [0-9a-fA-F]
$bin = [0-1]

tokens :-
  $white+				;
  $digit+				{ \p s -> TInt (read s) }
  ("0x" $hex+) 				{ \p s -> TInt (fst.head.readHex.(\(_:_:xs) -> xs) $ s) }
  ("0b" $bin+)				{ \p s -> TInt (foldl (\n c -> if c == '1' then n*2+1 else n*2) 0 $ (\(_:_:xs) -> xs) $ s) }

  [$alpha] [$alpha $digit]*		{ \p s -> TIdent s }

  "+"					{ \p s -> TPlus }
  "-"					{ \p s -> TMinus }
  "*"					{ \p s -> TMul }
  "/"					{ \p s -> TDiv }
  "%"					{ \p s -> TMod }
  "|"					{ \p s -> TBinOr }
  "&"					{ \p s -> TBinAnd }
  "^"					{ \p s -> TBinXor }
  "~"					{ \p s -> TBinNot }
  "!"					{ \p s -> TLogNot }
  "&&"					{ \p s -> TLogAnd }
  "||"					{ \p s -> TLogOr }
  "^^"					{ \p s -> TLogXor }
  "<<"					{ \p s -> TShl }
  ">>"					{ \p s -> TShr }
  "<<<"					{ \p s -> TRol }
  ">>>"					{ \p s -> TRor }

  "<"					{ \p s -> TLt }
  ">"					{ \p s -> TGt }
  "<="                                  { \p s -> TLe }
  ">="                                  { \p s -> TGe }
  "=="                                  { \p s -> TEq }
  "!="                                  { \p s -> TNe }

  "="					{ \p s -> TModifSet }
  "++"					{ \p s -> TModifInc }
  "--"					{ \p s -> TModifDec }
  "+="					{ \p s -> TModifPlus }
  "-="					{ \p s -> TModifMinus }
  "*="					{ \p s -> TModifMul }
  "/="					{ \p s -> TModifDiv }
  "%="					{ \p s -> TModifMod }
  "|="					{ \p s -> TModifBinOr }
  "&="					{ \p s -> TModifBinAnd }
  "^="					{ \p s -> TModifBinXor }
  ">>="					{ \p s -> TModifShr }
  "<<="					{ \p s -> TModifShl }
  ">>>="				{ \p s -> TModifRor }
  "<<<="				{ \p s -> TModifRol }
  "&&="					{ \p s -> TModifLogAnd }
  "||="					{ \p s -> TModifLogOr }
  "^^="					{ \p s -> TModifLogXor }

  "?"					{ \p s -> TQuestion }
  ":"					{ \p s -> TColon }
  ";" 					{ \p s -> TSemiColon }
  "("					{ \p s -> TLeftParen }
  ")"					{ \p s -> TRightParen }
{

data Token =
	TInt Int | TIdent String 
	| TPlus | TMinus | TMul | TDiv | TMod | TBinOr | TBinAnd | TBinXor | TBinNot | TRor | TRol
	| TLogNot | TLogAnd | TLogOr | TLogXor | TShl | TShr | TLt | TGt | TLe | TGe | TEq | TNe
	| TModifSet | TModifPlus | TModifMinus | TModifMul | TModifDiv | TModifMod | TModifRol | TModifRor
	| TModifShr | TModifShl | TModifLogAnd | TModifLogOr | TModifLogXor | TModifBinOr 
	| TModifBinAnd | TModifBinXor | TModifInc | TModifDec
	| TQuestion | TColon | TSemiColon | TLeftParen | TRightParen
	deriving (Eq)

instance Show Token where
  show x = case x of
    TInt i -> show i
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
    TModifLogAnd -> "&&="
    TModifLogOr -> "||="
    TModifLogXor -> "^^="
    TModifRol -> "<<<="
    TModifRor -> ">>>="
    TModifInc -> "++"
    TModifDec -> "--"
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
    TLogXor -> "^^"
    TShl -> "<<"
    TShr -> ">>"
    TRol -> "<<<"
    TRor -> ">>>"
    TLt -> "<"
    TGt -> ">"
    TLe -> "<="
    TGe -> ">="
    TEq -> "=="
    TNe -> "!="
    TQuestion -> "?"
    TColon -> ":"
    TSemiColon -> ";"
    TLeftParen -> "("
    TRightParen -> ")"

}
