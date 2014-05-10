module Tokens where

-- The token type:
data Token = TLambda
           | TEOF
           | TIf 
           | TThen 
           | TElse 
           | TTrue 
           | TFalse 
           | TBool
           | TPlus
           | TMinus
           | TUnderscore 
           | TApostrophe 
           | TDquote 
           | TBang 
           | THash 
           | TTriangle 
           | TStar 
           | TVbar 
           | TDot 
           | TSemi 
           | TComma 
           | TSlash 
           | TColon 
           | TColonColon 
           | TEq 
           | TEqEq 
           | TLSquare 
           | TLt 
           | TLCurly 
           | TLParen 
           | TLeftArrow 
           | TLCurlyBar 
           | TLSquareBar 
           | TRCurly 
           | TRParen 
           | TRSquare 
           | TGt 
           | TBarrCurly 
           | TBarGt 
           | TBarrSquare 
           | TColonEq 
           | TArrow 
           | TDArrow 
           | TDDArrow 
           | TTIdent String
           | TIdent String
           | TString String
           | TFloat Float
           | TInt Int
           deriving (Eq,Show)
