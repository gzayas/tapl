{-# LANGUAGE TypeSynonymInstances #-}
module Pretty where
import Text.PrettyPrint
import AbsSyn
import Support

class PP a where
  toDoc :: Context -> a -> Doc

instance PP Ty where
  toDoc ctx (TyArr ty1 ty2) = toDoc ctx ty1 <> text " -> " <> toDoc ctx ty2
  toDoc ctx TyBool = text "Bool"
  
obox0 = empty
obox = empty
cbox = empty

ctoDoc :: Context -> Doc
ctoDoc ctx = lbrace <> hsep (map pr ctx) <> rbrace
  where
    pr (s,b) = lparen <> text s <> colon <> (text $ show b) <> rparen


small (TmVar _ _) = True
small _ = False

instance PP Term where
  toDoc ctx (TmVar x n) | ctxLength ctx == n = text $ either (\l -> "") (\r -> r) (indexToName noSrcLoc ctx x)
                        | otherwise          = text ("[bad index: " ++ (show x) ++
                                                       "/" ++ (show n) ++ " in ") <> ctoDoc ctx <> rbrack
  toDoc ctx (TmAbs x tyT1 t2) = obox <> text "(lambda " <> text x' <>
                                colon <> toDoc ctx tyT1 <> text "." <> space <>
                                toDoc ctx' (unLoc t2) <> text ")" <>
                                cbox
    where
      (ctx',x') = pickFreshName ctx x
  toDoc ctx (TmApp t1 t2) = obox0 <> toDoc ctx (unLoc t1) <> space <> toDoc ctx (unLoc t2) <> cbox
  toDoc ctx (TmTrue) = text "true"
  toDoc ctx (TmFalse) = text "false"
  toDoc ctx (TmIf t1 t2 t3)  =
         obox0 <> text "if " <> toDoc ctx (unLoc t1) <>
         space <> text "then " <> toDoc ctx (unLoc t2) <> 
         space <> text "else " <> toDoc ctx (unLoc t3) <>
         cbox

instance PP Binding where
  toDoc ctx NameBind = empty
  toDoc ctx (VarBind ty) = colon <> toDoc ctx ty



pp :: Context -> Term -> String
pp ctx t = render $ toDoc ctx t

pretty :: Context -> Term -> Ty -> String
pretty ctx t tyT = render $ hcat [toDoc ctx t,
                                  text " : ",
                                  toDoc ctx tyT,
                                  text "\n"]

prettyBinding :: Context -> String -> Binding -> String
prettyBinding ctx x bind = render $ hcat [text x, toDoc ctx bind]

