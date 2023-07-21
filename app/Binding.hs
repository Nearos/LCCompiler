{-# LANGUAGE TypeFamilies #-}
module Binding where

import qualified Data.Set as S
import Control.Monad.Trans.State ( evalState, get, put, State )
import Text.Parsec (SourcePos)


import Tree
import Parser ( Parsed ) 
import Data.List
import Data.Bifunctor (Bifunctor(bimap))

-- 

data ResolvedSymbol
    = Free String
    | Bound String Int

type Binding = (String, Int)

bindingToBound :: Binding -> ResolvedSymbol
bindingToBound (str, n) = Bound str n

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

data BindInfo = BindInfo {
    pos :: SourcePos,
    id :: Int 
}

type instance LambdaMeta Resolved = ResolvedLambda
type instance VarMeta Resolved = SourcePos
type instance AppMeta Resolved = SourcePos
type instance ExternMeta Resolved = SourcePos
type instance LitMeta Resolved = SourcePos
type instance BindMeta Resolved = BindInfo
type instance ConsMeta Resolved = SourcePos
type instance Const Resolved = ResolvedSymbol

instance ShowablePass ResolvedSymbol where
    passShow (Free str) = "'" ++ str ++ "'"
    passShow (Bound str id) = " "++str ++ "@" ++ show id ++" "

instance ShowablePass ResolvedLambda where
    passShow (ResolvedLambda caps pos) = show pos ++ ": " ++ intercalate "," (map show caps)


instance ShowablePass BindInfo where 
    passShow (BindInfo pos id) = "@" ++ show pos ++ " #" ++ show id

-- function

generateConstructors :: [Toplevel Parsed] -> Resolver [Toplevel Parsed]
generateConstructors [] = return []
generateConstructors (binding@Binding {}:rest) = (binding :) <$> generateConstructors rest
generateConstructors (constDef@(ConstDef meta name arity) : rest) = do 
        constructor <- constructor
        rest <- generateConstructors rest
        return $ constDef : constructor : rest
    where 
        constructor = Binding meta name <$> genConstructor arity []

        genConstructor :: Int -> [String] -> Resolver (Expr Parsed)
        genConstructor 0 vars = return $ Construct meta name $ map (Var meta) $ reverse vars
        genConstructor n vars = do 
            id <- getId
            let varName = "@constructor_arg" ++ show id 
            inner <- genConstructor (n-1) $ varName : vars
            return $ Lambda meta varName inner



resolveProgram :: [Binding] -> [Toplevel Parsed] -> Resolver [Toplevel Resolved]

resolveProgram _ [] = return []

resolveProgram bindings (Binding meta name value:defs) = do
    newId <- if name == "main" then return $ -1 else getId
    let newBindings = (name, newId) : bindings
    newValue <- resolve newBindings value -- generate body with new bindings to allow recursion
    restResolved <- resolveProgram newBindings defs
    let info = BindInfo meta newId
    return $ Binding info (Bound name newId) newValue : restResolved

resolveProgram bindings (ConstDef meta name arity : rest) = do 
    newId <- getId 
    let newBindings = (name, newId) : bindings
    let info = BindInfo meta newId
    ctor <- constructor info newId 
    restResolved <- resolveProgram newBindings rest
    return $ 
        ConstDef info (Bound name newId) arity 
        : ctor
        : restResolved
    where 
        constructor info newId = Binding info (Bound name newId) <$> genConstructor newId arity []

        genConstructor :: Int -> Int -> [Binding] -> Resolver (Expr Resolved)
        genConstructor newId 0 vars = return $ Construct meta (Bound name newId) $ map (Var meta . bindingToBound) $ reverse vars
        genConstructor newId n vars = do 
            id <- getId
            let strName = "'constructor_arg"
            let varName = Bound strName id
            inner <- genConstructor newId (n-1) $ (strName, id) : vars
            let newMeta = ResolvedLambda [] meta 
            return $ Lambda newMeta varName inner


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
capture (Construct meta name subexprs) = bimap (foldl S.union S.empty) (Construct meta name) $ unzip $ map capture subexprs
capture fv = (S.empty, fv)

captureProgram :: [Toplevel Resolved] -> [Toplevel Resolved]
captureProgram = map captureBinding
    where
        captureBinding (Binding meta name expr) = Binding meta name $ snd $ capture expr
        captureBinding a = a

{-# DEPRECATED #-}
resolveBindings :: Expr Parsed -> Expr Resolved
resolveBindings = snd . capture . runResolver . resolve []

bindProgram :: [Toplevel Parsed] -> [Toplevel Resolved]
bindProgram = captureProgram . runResolver . resolveProgram []