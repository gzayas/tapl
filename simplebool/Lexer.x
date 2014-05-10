
{
module Lexer (scanAll,get_tok, lex_tok) where

import Control.Monad.State (StateT, get)
import Tokens
import ParserM (P (..), StartCode, alexStartCode,
                alexSetStartCode, Action, Located, mkEOF, mkT, mkTIdent, 
                mkIdent, mkString, mkFloat, mkInt,
                showSrcLoc, alexSrcLoc, alexInp,
                AlexInput, alexGetByte, alexInputPrevChar)
}

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$digit = 0-9
$large = [A-Z \xc0-\xd6 \xd8-\xde]
$small = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

@decimal = $digit+
@exponent = [eE] [\-\+] @decimal


$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

$idchar = [$alpha $digit \']
@varid  = $small $idchar*
@tyid  = $large $idchar*

tokens :-
 <0>   $white+		;
 <0>   "--".*           ; 
-- Keywords
 <0> lambda  { mkT TLambda }
 <0> if      { mkT TIf }
 <0> then    { mkT TThen }
 <0> else    { mkT TElse }
 <0> true    { mkT TTrue }
 <0> false   { mkT TFalse }
 <0> Bool    { mkT TBool }
  
-- Symbols 
 <0> \+    { mkT TPlus }
 <0> \-    { mkT TMinus }
 <0> \_    { mkT TUnderscore }
 <0> \'    { mkT TApostrophe }
 <0> \"    { mkT TDquote }
 <0> \!    { mkT TBang }
 <0> \#    { mkT THash }
 <0> \$    { mkT TTriangle }
 <0> \*    { mkT TStar }
 <0> \|    { mkT TVbar }
 <0> \.    { mkT TDot }
 <0> \;    { mkT TSemi }
 <0> \,    { mkT TComma }
 <0> \/    { mkT TSlash }
 <0> \:    { mkT TColon }
 <0> "::"  { mkT TColonColon }
 <0> \=    { mkT TEq }
 <0> "=="  { mkT TEqEq }
 <0> \[    { mkT TLSquare }
 <0> \<    { mkT TLt }
 <0> \{    { mkT TLCurly }
 <0> \(    { mkT TLParen }
 <0> "<-"  { mkT TLeftArrow }
 <0> "{|"  { mkT TLCurlyBar }
 <0> "[|"  { mkT TLSquareBar }
 <0> \}    { mkT TRCurly }
 <0> \)    { mkT TRParen }
 <0> \]    { mkT TRSquare }
 <0> \>    { mkT TGt }
 <0> "|}"  { mkT TBarrCurly }
 <0> "|>"  { mkT TBarGt }
 <0> "|]"  { mkT TBarrSquare }

-- Special compound symbols
 <0> ":="   { mkT TColonEq }
 <0> "->"   { mkT TArrow }
 <0> "=>"   { mkT TDArrow }
 <0> "==>"  { mkT TDDArrow }

-- other
 <0>  @tyid		                { mkTIdent }
 <0>  @varid		                { mkIdent }
 <0>  \" @string* \"      	  	{ mkString }
 <0>  @decimal \. @decimal @exponent?
    | @decimal @exponent		{ mkFloat }
 <0>  @decimal                          { mkInt }

{
get_tok :: AlexInput -> Either String (Located Token, AlexInput)
get_tok = \i ->
   do case alexScan i (alexStartCode i) of
          AlexEOF -> mkEOF (i, "")
          AlexError _ -> fail $ "Lexical error at " ++ showSrcLoc (alexSrcLoc i)
          AlexSkip i' _ -> get_tok i'
          AlexToken i' l a -> a (i', take l (alexInp i))

lex_tok :: (Located Token -> P a) -> P a
lex_tok cont = P $ \i ->
   do (tok, iz) <- get_tok i
      case cont tok of
          P x -> x iz

scanAll :: AlexInput -> [String]
scanAll i = go i (alexScan i 0) []
  where	
    go i AlexEOF tks          = reverse tks 
    go i (AlexError _) _      = error $ "Lexical error at " ++ showSrcLoc (alexSrcLoc i)
    go i (AlexSkip i' _)  tks = go i' (alexScan i' 0) tks
    go i (AlexToken i' l a)tks= go i' (alexScan i' 0) ((take l (alexInp i)):tks)
}

