{-# LANGUAGE TypeFamilies #-}
module Binding where 

import qualified Data.Set as S
import Control.Monad.Trans.State ( evalState, get, put, State )
import Text.Parsec (SourcePos)


import Tree
import Parser (Parsed)

-- 

data ResolvedSymbol 
    = Free String 
    | Bound String Int 

type Binding = (String, Int)

-- Id generator monad

type Resolver = State Int

getId :: Resolver Int 
getId = do 
    ret <- get 
    put $ ret + 1
    return ret

runResolver :: Resolver a -> a
runResolver r = evalState r 0

-- tree 

data Resolved 

type instance Sym Resolved = ResolvedSymbol

data ResolvedLambda 
    = ResolvedLambda {
        captures :: [Binding],
        position :: SourcePos 
    }

type instance LambdaMeta Resolved = ResolvedLambda
type instance VarMeta Resolved = SourcePos
type instance AppMeta Resolved = SourcePos
type instance ExternMeta Resolved = SourcePos
type instance LitMeta Resolved = SourcePos
type instance BindMeta Resolved = SourcePos

instance ShowablePass ResolvedSymbol where 
    passShow (Free str) = "'" ++ str ++ "'"
    passShow (Bound str id) = " "++str ++ "@" ++ show id ++" "

instance ShowablePass ResolvedLambda where
    passShow (ResolvedLambda _ pos) = show pos

-- function

resolveProgram :: [Binding] -> [Toplevel Parsed] -> Resolver [Toplevel Resolved]
resolveProgram _ [] = return [] 
resolveProgram bindings (Binding meta name value:defs) = do 
    newId <- getId
    let newBindings = (name, newId) : bindings
    newValue <- resolve newBindings value -- generate body with new bindings to allow recursion
    restResolved <- resolveProgram newBindings defs 
    return $ Binding meta (Bound name newId) newValue : restResolved

resolve :: [Binding] -> Expr Parsed -> Resolver (Expr Resolved)

resolve bindings (Lambda meta var body) = do 
    newId <- getId 
    resolvedBody <- resolve ((var, newId) : bindings) body 
    let meta' = ResolvedLambda [] meta 
    return $ Lambda meta' (Bound var newId) resolvedBody

resolve bindings (App meta a b) 
    = App meta <$> resolve bindings a <*> resolve bindings b

resolve bindings (Var meta name ) 
    = pure $ case lookup name bindings of 
        Just id -> Var meta $ Bound name id
        Nothing -> Var meta $ Free name 

resolve _ (Extern externMeta name) = pure $ Extern externMeta name
resolve _ (SrcStringLit meta name) = pure $ SrcStringLit meta name 
resolve _ (SrcIntLit meta name) = pure $ SrcIntLit meta name

capture :: Expr Resolved -> (S.Set Binding, Expr Resolved) 
capture (Lambda (ResolvedLambda _ pos) (Bound name id) subexpr) 
    = (captures, Lambda captureMeta (Bound name id) sub)
    where 
        (subcap, sub) = capture subexpr
        captures = subcap S.\\ S.singleton (name, id)
        captureMeta = ResolvedLambda (S.toList captures) pos

capture (App meta a b) = (aCap `S.union` bCap, App meta a' b')
    where 
        (aCap, a') = capture a 
        (bCap, b') = capture b 

capture (Var meta (Bound name str)) = (S.singleton (name, str), Var meta (Bound name str))
capture fv = (S.empty, fv)

captureProgram :: [Toplevel Resolved] -> [Toplevel Resolved]
captureProgram = map captureBinding 
    where 
        captureBinding (Binding meta name expr) = Binding meta name $ snd $ capture expr

{-# DEPRECATED #-}
resolveBindings :: Expr Parsed -> Expr Resolved
resolveBindings = snd . capture . runResolver . resolve []

bindProgram :: [Toplevel Parsed] -> [Toplevel Resolved]
bindProgram = captureProgram . runResolver . resolveProgram []