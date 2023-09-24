{-# LANGUAGE TypeFamilies #-}
module Binding where

import qualified Data.Set as S
import Control.Monad.Trans.State ( evalState, get, put, State, StateT (runStateT), evalStateT, modify )
import Text.Parsec (SourcePos)


import Tree
import Parser ( Parsed )
import Data.List
import Data.Bifunctor (Bifunctor(bimap))
import Control.Monad (forM)
import Data.Maybe


import Typing.Typ(Typ (TyCon, TyVar, TyApp, TyForall), TySym)
import qualified Typing.Parser as TyParser
import Control.Monad.Trans.Class
-- 

data ResolvedSymbol
    = Free String
    | Bound String Int
    deriving (Eq, Ord)

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
        position :: SourcePos,
        rec_bind :: Maybe Binding
    }

data BindInfo = BindInfo {
    pos :: SourcePos,
    id :: Int,
    boundTyp :: Maybe (Typ Resolved)
}

type instance LambdaMeta Resolved = ResolvedLambda
type instance VarMeta Resolved = SourcePos
type instance AppMeta Resolved = SourcePos
type instance ExternMeta Resolved = SourcePos
type instance LitMeta Resolved = SourcePos
type instance BindMeta Resolved = BindInfo
type instance ConsMeta Resolved = SourcePos
type instance Const Resolved = ResolvedSymbol
type instance CaseMeta Resolved = SourcePos
type instance TySym Resolved = ResolvedSymbol

instance ShowablePass ResolvedSymbol where
    passShow (Free str) = "'" ++ str ++ "'"
    passShow (Bound str id) = " "++str ++ "@" ++ show id ++" "

instance ShowablePass ResolvedLambda where
    passShow (ResolvedLambda caps pos rec_bind) = show pos ++ ": " ++ intercalate "," (map show caps) ++ maybe "" ((", " ++) . show) rec_bind


instance ShowablePass BindInfo where
    passShow (BindInfo pos id typ) = "@" ++ show pos ++ " #" ++ show id

-- function

resolveType :: Typ TyParser.Parsed -> Resolver (Typ Resolved)
resolveType typ = do 
    (resolved, bindings) <- runStateT (go typ) [] 
    let withForalls = foldl (\typ (name, id) -> TyForall (Bound name id) typ) resolved bindings
    pure withForalls
    where
        
        go :: Typ TyParser.Parsed -> StateT [Binding] Resolver (Typ Resolved)
        go (TyCon a) = return $ TyCon $ Free a
        go (TyVar a) = do 
            currentBindings <- get 
            case lookup a currentBindings of 
                Just val -> pure $ TyVar $ Bound a val 
                Nothing -> do 
                    newId <- lift getId
                    modify ((a, newId) : )
                    pure $ TyVar $ Bound a newId
        
        go (TyApp a b) = do 
            aB <- go a 
            bB <- go b
            pure $ TyApp aB bB


resolveProgram :: [Binding] -> [Toplevel Parsed] -> Resolver [Toplevel Resolved]

resolveProgram _ [] = return []

resolveProgram bindings (Binding (meta, typDecl) name value:defs) = do
    newId <- if name == "main" then return $ -1 else getId
    let newBindings = (name, newId) : bindings
    boundValue <- resolve newBindings value -- generate body with new bindings to allow recursion
    let recValue = case boundValue of
            Lambda (ResolvedLambda a b _) c d -> Lambda (ResolvedLambda a b $ Just (name, newId)) c d
            a -> a
    restResolved <- resolveProgram newBindings defs
    boundTyp <- traverse resolveType typDecl
    let info = BindInfo meta newId boundTyp
    return $ Binding info (Bound name newId) recValue : restResolved

resolveProgram bindings (ConstDef (meta, typDecl) name arity : rest) = do
    newId <- getId
    let newBindings = (name, newId) : bindings
    boundTyp <- traverse resolveType typDecl
    let info = BindInfo meta newId boundTyp
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
            let newMeta = ResolvedLambda [] meta Nothing
            return $ Lambda newMeta varName inner


resolve :: [Binding] -> Expr Parsed -> Resolver (Expr Resolved)

resolve bindings (Lambda meta var body) = do
    newId <- getId
    resolvedBody <- resolve ((var, newId) : bindings) body
    let meta' = ResolvedLambda [] meta Nothing
    return $ Lambda meta' (Bound var newId) resolvedBody

resolve bindings (Case meta scrut branches) = Case meta <$> resolve bindings scrut <*> mapM resolveBranch branches
    where
        resolveBranch (CaseBranch pat expr) = do
            (newBindings, newPattern) <- patternBindings pat
            CaseBranch newPattern <$> resolve newBindings expr

        patternBindings (Pattern const subs) = do
                newBindings <- forM subs $ \ sym -> do
                    newId <- getId
                    return (sym, newId)
                let boundConst = case lookup const bindings of
                        Just id -> Bound const id
                        Nothing -> Free const
                return (newBindings ++ bindings, Pattern boundConst $ map bindingToBound newBindings)

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
capture (Lambda (ResolvedLambda _ pos rec_bind) (Bound name id) subexpr)
    = (captures, Lambda captureMeta (Bound name id) sub)
    where
        (subcap, sub) = capture subexpr
        captures = subcap S.\\ S.singleton (name, id)
        captureMeta = ResolvedLambda (S.toList captures) pos rec_bind

capture (Case meta scrut branches) = (scrutCaptures `S.union` branchCaptures, Case meta newScrut newBranches)
    where
        (scrutCaptures, newScrut) = capture scrut

        (branchCaptureSets, newBranches) = unzip $ map captureBranch branches

        branchCaptures = foldl S.union S.empty branchCaptureSets

        captureBranch (CaseBranch pat@(Pattern _ binds) expr) =
            let
                (exprCaptures, newExpr) = capture expr
                patternBinds = foldr (S.union . symbolToBindingSet) S.empty binds
            in (exprCaptures S.\\ patternBinds, CaseBranch pat newExpr)

        symbolToBindingSet (Free _) = S.empty
        symbolToBindingSet (Bound a b) = S.singleton (a, b)


capture (App meta a b) = (aCap `S.union` bCap, App meta a' b')
    where
        (aCap, a') = capture a
        (bCap, b') = capture b

capture (Var meta (Bound name str)) = (S.singleton (name, str), Var meta (Bound name str))
capture (Construct meta name subexprs) = bimap (foldl S.union S.empty) (Construct meta name) $ unzip $ map capture subexprs
capture fv@(Var _ (Free _)) = (S.empty, fv)
capture il@(SrcIntLit {}) = (S.empty, il)
capture sl@(SrcStringLit {}) = (S.empty, sl)

captureProgram :: [Toplevel Resolved] -> [Toplevel Resolved]
captureProgram = map captureBinding
    where
        captureBinding (Binding meta (Bound name id) expr) =
            let captured = capture expr
            in Binding meta (Bound name id) $ snd captured
        captureBinding a = a

{-# DEPRECATED #-}
resolveBindings :: Expr Parsed -> Expr Resolved
resolveBindings = snd . capture . runResolver . resolve []

bindProgram :: [Toplevel Parsed] -> [Toplevel Resolved]
bindProgram = captureProgram . runResolver . resolveProgram []