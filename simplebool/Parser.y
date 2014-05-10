
{
  module Parser where

import Lexer (lex_tok)
import Tokens
import AbsSyn
import Support
import ParserM (P, runP, Located(..), getLoc, getSrcLoc, showSrcLoc, happyError)
import Debug.Trace
}

%name      parsex toplevel
%name      termexs terms
%name      termex term
%name      typex  type
%tokentype { (Located Token) }
%monad     { P }
%lexer     { lex_tok } { L _ TEOF }

%token
 TEOF    { L _ TEOF }
 lambda  { L _ TLambda }
 if      { L _ TIf }
 then    { L _ TThen }
 else    { L _ TElse }
 true    { L _ TTrue }
 false   { L _ TFalse }
 bool    { L _ TBool }
-- Symbols 
  '_'    { L _ TUnderscore }
  '\''   { L _ TApostrophe }
  '"'    { L _ TDquote }
  '!'    { L _ TBang }
  '#'    { L _ THash }
  '$'    { L _ TTriangle }
  '*'    { L _ TStar }
  '|'    { L _ TVbar }
  '.'    { L _ TDot }
  ';'    { L _ TSemi }
  ','    { L _ TComma }
  '/'    { L _ TSlash }
  ':'    { L _ TColon }
  "::"   { L _ TColonColon }
  '='    { L _ TEq }
  "=="   { L _ TEqEq }
  '['    { L _ TLSquare }
  '<'    { L _ TLt }
  '{'    { L _ TLCurly }
  '('    { L _ TLParen }
 "<-"    { L _ TLeftArrow }
  "{|"   { L _ TLCurlyBar }
  "[|"   { L _ TLSquareBar }
  '}'    { L _ TRCurly }
  ')'    { L _ TRParen }
  ']'    { L _ TRSquare }
  '>'    { L _ TGt }
  "|}"   { L _ TBarrCurly }
  "|>"   { L _ TBarGt }
  "|]"   { L _ TBarrSquare }

-- Special compound symbols
  ":="   { L _ TColonEq }
  "->"   { L _ TArrow }
  "=>"   { L _ TDArrow }
  "==>"  { L _ TDDArrow }
  
  tyid	 { L _ (TTIdent _) }
  varid	 { L _ (TIdent _) }
  string { L _ (TString _) }
  float	 { L _ (TFloat _) }
  int    { L _ (TInt _) }

%%

toplevel ::  { [LCommand] }
toplevel : TEOF { [] }
         | command ';' { [$1] }
         | command ';' toplevel { ($1:$3) }

-- A top-level command
command :: { LCommand }
command : term  { l1 $1 (Eval $1) }
        | varid binder { l1 $1 (Bind (getVarId $1) $2) }

terms :: { [LTerm] }
terms : TEOF { [] }
      | term ';' { [$1] }
      | term ';' terms { ($1:$3) }
        
-- Right-hand sides of top-level bindings
binder :: { LBinding }
binder : ':' type { l1 $2 (VarBind $ unLoc $2) }

-- All type expressions
type :: { LTy }
type : arrowType  { $1 }

-- Atomic types are those that never need extra parentheses
aType :: { LTy }
aType : '(' type ')' { $2 } 
      | bool         { l1 $1 TyBool }

-- An "arrow type" is a sequence of atomic types separated by arrows.
arrowType :: { LTy }
arrowType : aType "->" arrowType { l1 $1 (TyArr (unLoc $1) (unLoc $3)) }
          | aType                { $1 }

term :: { LTerm }
term : appTerm { $1 }
     | lambda varid ':' type '.' term { l1 $1 $ TmAbs (getVarId $2) (unLoc $4) $6 }
     | lambda '_'   ':' type '.' term { l1 $1 $ TmAbs "_" (unLoc $4) $6 }
     | if term then term else term    { l1 $1 $ TmIf $2 $4 $6 }

appTerm :: { LTerm }
appTerm : aTerm { $1 }
        | appTerm aTerm  { l1 $1 (TmApp $1 $2) }

-- Atomic terms are ones that never require extra parentheses
aTerm :: { LTerm }
aTerm : '(' term ')' { $2 } 
      | varid        { l1 $1 (TmVarU $ getVarId $1)}
      | true         { l1 $1 TmTrue }
      | false        { l1 $1 TmFalse }


{

l1 f = sL (getLoc f)
-- strict constructor version:
{-# INLINE sL #-}
sL :: SrcLoc -> a -> Located a
sL l a = l `seq` a `seq` L l a

getVarId  (L _ (TIdent x))  = x
getTyId	  (L _ (TTIdent x)) = x
getString (L _ (TString x)) = x
getFloat  (L _ (TFloat x))  = x
getInt    (L _ (TInt x))    = x

parse :: String -> String -> Either String [LCommand]
parse fileName s = runP fileName s parsex

termit :: String -> String -> Either String LTerm
termit fileName s = runP fileName s termex

termsit :: String -> String -> Either String [LTerm]
termsit fileName s = runP fileName s termexs

tyit :: String -> String -> Either String LTy
tyit fileName s = runP fileName s typex

}

