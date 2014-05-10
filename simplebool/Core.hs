module Core where
import AbsSyn
import Support


isVal :: Context -> Term -> Bool
isVal _ TmTrue  = True
isVal _ TmFalse = True
isVal _ (TmAbs _ _ _) = True
isVal _  _ = False


eval1 :: Context -> LTerm -> Maybe LTerm
eval1 ctx (L _ (TmApp (L _ (TmAbs x tyT11 t12)) v2)) | isVal ctx $ unLoc v2 = Just $ termSubstTop v2 t12
eval1 ctx (L l (TmApp v1 t2)) | isVal ctx $ unLoc v1 = do
  t2' <- eval1 ctx t2
  return $ L l (TmApp v1 t2')
eval1 ctx (L l (TmApp t1 t2)) = do
  t1' <- eval1 ctx t1
  return $ L l (TmApp t1'  t2)
eval1 ctx (L l (TmIf (L _ TmTrue) t2 t3)) = Just t2
eval1 ctx (L l (TmIf (L _ TmFalse) t2 t3)) = Just t3
eval1 ctx (L l (TmIf t1 t2 t3)) = do
  t1' <- eval1 ctx t1
  return $ L l (TmIf t1' t2 t3)
eval1 ctx _ = Nothing

eval :: Context -> LTerm -> LTerm
eval ctx t = go t (eval1 ctx t)
  where
    go t Nothing = t
    go t (Just t') = go t' (eval1 ctx t')


-- typing
typeOf :: Context -> LTerm -> Either String Ty
typeOf ctx (L l (TmVar i _)) = getTypeFromContext l ctx i
typeOf ctx (L _ (TmAbs x tyT1 t2)) = do
  tyT2 <- typeOf ctx' t2
  return $ TyArr tyT1  tyT2
  where
    ctx' = addBinding ctx x (VarBind tyT1)
typeOf ctx (L _ (TmApp t1 t2)) = do
  tyT1 <- typeOf ctx t1
  tyT2 <- typeOf ctx t2
  checkArrType tyT1 tyT2
  where
    checkArrType (TyArr tyT11 tyT12) tyT2 | tyT2 == tyT11 = Right tyT12
                                          | otherwise = Left "parameter type mismatch"
    checkArrType _ _ = Left "arrow type expected"
typeOf ctx (L _ TmTrue)  = return TyBool
typeOf ctx (L _ TmFalse) = return TyBool
typeOf ctx (L _ (TmIf t1 t2 t3)) | (typeOf ctx t1) == Right TyBool = do
  tyT2 <- typeOf ctx t2
  if Right tyT2 == (typeOf ctx t3)
    then return tyT2
    else Left "arms of conditional have different types"
                                 | otherwise = Left "guard of conditional not a boolean"



