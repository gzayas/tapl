{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where
import System.Environment
import Control.Monad
import Parser
import Support
import Core
import Pretty
import AbsSyn


main :: IO ()
main = do
  args <- getArgs
  let input = head args
  s <- readFile input
  let r = parse input s
  mapM_ putStr $ process r
    where
      process (Left e) = [e]
      process (Right cmds) = processCommands ctx cmds'
        where
          (ctx, cmds') = prepContext emptyContext cmds

processCommands :: Context -> [LCommand] -> [String]
processCommands ctx cmds = go ctx cmds []
  where
    go ctx [] res = reverse res
    go ctx (c:cs) res = go ctx' cs (r:res)
      where
        (ctx',r) = processCommand ctx $ unLoc c

processCommand :: Context -> Command -> (Context, String)
processCommand ctx (Eval t) = go $ typeOf ctx t
  where
    go (Left e) = (ctx, e)
    go (Right tyT) = (ctx, pretty ctx t' tyT)
    t' = unLoc $ eval ctx t
    
processCommand ctx (Bind x bind) =  (ctx', prettyBinding ctx x $ unLoc bind)
  where
    ctx' = addBinding ctx x $ unLoc bind

prepContext :: Context -> [LCommand] -> (Context, [LCommand])
prepContext ctx cmds = go ctx cmds []
  where
    go :: Context -> [LCommand] -> [LCommand] -> (Context, [LCommand])
    go ctx [] cmds' = (ctx, reverse cmds')
    go ctx (c:cmds) cmds' = go ctx' cmds (c':cmds')
      where
        (ctx', c') = prepCommand ctx c

prepCommand :: Context -> LCommand -> (Context, LCommand)
prepCommand ctx (L l (Eval lt))  = (ctx, L l (Eval lt'))
  where
    (ctx', lt') = prepTerm ctx lt
prepCommand ctx (L l b@(Bind n _)) = (addName ctx n, L l b)

prepTerm :: Context -> LTerm -> (Context, LTerm)
prepTerm ctx (L l (TmAbs n ty t))  = (ctx', L l $ TmAbs n ty t')
  where
    (ctx', t') = prepTerm (addName ctx n) t
prepTerm ctx (L l (TmApp t1 t2))   = (ctx, L l $ TmApp t1' t2')
  where
    (_, t1') = (prepTerm ctx t1)
    (_, t2') = (prepTerm ctx t2)
prepTerm ctx (L l (TmIf t1 t2 t3)) = (ctx, L l $ TmIf  t1' t2' t3')
  where
    (_, t1') = (prepTerm ctx t1)
    (_, t2') = (prepTerm ctx t2)
    (_, t3') = (prepTerm ctx t3)
prepTerm ctx (L l (TmVarU n)) = (ctx, L l (TmVar (nameToIndex l ctx n) (ctxLength ctx)))
prepTerm ctx l@(L _ TmTrue) = (ctx, l)
prepTerm ctx l@(L _ TmFalse) = (ctx, l)
