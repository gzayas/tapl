module AbsSyn where
import Support


data Ty = TyArr Ty Ty | TyBool deriving (Show, Eq)

data Term = TmVarU String  -- String variable created at parsed time. It will be replaced by its de-brujin indices once terms are processed through the initial environment
          | TmVar Int Int
          | TmAbs String Ty LTerm
          | TmApp LTerm LTerm
          | TmTrue 
          | TmFalse
          | TmIf LTerm LTerm LTerm
          deriving Show

data Binding =  NameBind | VarBind Ty
             deriving Show
                      
type Context = [(String, Binding)]

data Command = Eval LTerm
             | Bind String LBinding
             deriving Show

type LTerm = Located Term
type LTy = Located Ty
type LBinding = Located Binding
type LCommand = Located Command
                      
-- Context management 

emptyContext = []

ctxLength ctx = length ctx


addBinding :: Context -> String -> Binding -> Context
addBinding ctx x bind = (x,bind) : ctx

addName ctx x = addBinding ctx x NameBind

isNameBound [] x = False
isNameBound ((y,_):ys) x | y == x = True
                         | otherwise = isNameBound ys x

pickFreshName :: Context -> String -> (Context, String)
pickFreshName ctx x | isNameBound ctx x = pickFreshName ctx (x ++ "'")
                    | otherwise = (((x,NameBind):ctx), x)

indexToName :: SrcLoc -> Context -> Int -> Either String String
indexToName l ctx x | length ctx > x = Right $ fst (ctx!!x)
                    | otherwise = Left ("Variable lookup failure: offset: "
                                        ++ show x ++", ctx size: " ++ show (length ctx))

nameToIndex :: SrcLoc -> Context -> String -> Int
nameToIndex l ctx x = go l ctx x 0
  where
    go :: SrcLoc -> Context -> String -> Int -> Int
    go l [] x _ = error $ "Identifier " ++ x ++ " is unbound"
    go l ((y,_):ys) x idx | y==x = idx
                          | otherwise = go l ys x (idx+1)


-- Shifting

tmmap :: (SrcLoc -> Int -> Int -> Int -> LTerm) -> Int -> LTerm -> LTerm
tmmap onvar c t = walk c t
  where
    walk c (L l (TmVar x n)) = onvar l c x n
    walk c (L l (TmAbs x tyT1 t2))= L l $ TmAbs x tyT1 (walk (c+1) t2)
    walk c (L l (TmApp t1 t2))    = L l $ TmApp (walk c t1) (walk c t2)
    walk c t@(L l TmTrue)           = t
    walk c t@(L l TmFalse)          = t
    walk c (L l (TmIf t1 t2 t3))    = L l $ TmIf (walk c t1) (walk c t2) (walk c t3)

termShiftAbove :: Int -> Int -> LTerm -> LTerm
termShiftAbove d c t =
  tmmap
    (\l c x n -> if x>=c then L l (TmVar (x+d) (n+d)) else L l (TmVar x (n+d)))
    c t

termShift :: Int -> LTerm -> LTerm
termShift d t = termShiftAbove d 0 t

-- Substitution
termSubst :: Int -> LTerm -> LTerm -> LTerm
termSubst j s t =
  tmmap
    (\l j x n -> if x==j then termShift j s else L l (TmVar x n))
    j t

termSubstTop :: LTerm -> LTerm -> LTerm
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)


-- Context management (continued)

getBinding :: SrcLoc -> Context -> Int -> Either String Binding
getBinding l ctx i | length ctx > i = Right $ snd (ctx!!i)
                   | otherwise = Left ("Variable lookup failure: offset: "
                                          ++ show i ++", ctx size: " ++ show (length ctx))

getTypeFromContext :: SrcLoc -> Context -> Int -> Either String Ty
getTypeFromContext l ctx i = either (\l -> Left l) (\r -> getTy r) $ getBinding l ctx i
  where
    getTy (VarBind tyT) = Right tyT
    getTy  _ = Left ("getTypeFromContext: Wrong kind of binding for variable " ++ name)
    Right name = indexToName l ctx i

    
  --tmSrcLoc (TmAbs l _ _ _) = l
--tmSrcLoc (TmApp l _ _)   = l
--tmSrcLoc (TmTrue l)      = l
--tmSrcLoc (TmFalse l)     = l
--tmSrcLoc (TmIf l _ _ _)  = l


