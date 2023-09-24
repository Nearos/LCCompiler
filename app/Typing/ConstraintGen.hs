module Typing.ConstraintGen where

import Typing.Environment
import Typing.Constraint
import Typing.Typ
import Typing.Parser (Parsed)
import Typing.Pass

import Tree
import Binding
import Control.Monad
import Data.Data (TyCon)
import Data.Foldable (foldlM)

tcTypeDecl :: Typ Resolved -> TyEnv (Typ Typing)
tcTypeDecl (TyCon (Free a)) = pure $ TyCon $ Const a
tcTypeDecl (TyVar bnd) = lookupSymbol bnd
tcTypeDecl (TyApp a b) = TyApp <$> tcTypeDecl a <*> tcTypeDecl b
tcTypeDecl (TyForall x t) = scope $ do 
    tv <-  getTyVar
    addSymbol x $ TyVar tv
    nest <- tcTypeDecl t 
    pure $ TyForall tv nest

genConstraintsPattern :: Typ Typing -> Pattern Resolved -> TyEnv (Pattern Typing)
genConstraintsPattern typ (Pattern cons args) = do 
    consTyp <- lookupSymbol cons
    patternType <- foldlM (\soFa sym -> do 
        passOnTyp <- TyVar <$> getTyVar
        thisSymTyp <- lookupSymbol sym 
        emitConstraint $ TyEq soFa $ TyApp (TyApp (TyCon (Const "->") ) thisSymTyp) passOnTyp
        pure passOnTyp
        )
        consTyp
        args
    emitConstraint $ TyEq typ patternType
    pure $ Pattern cons args  

genConstraintsExpr :: Typ Typing -> Expr Resolved -> TyEnv (Expr Typing)

genConstraintsExpr typ (App meta a b) = do
    bTyp <- TyVar <$> getTyVar
    let funTyp = TyApp (TyApp (TyCon (Const "->")) bTyp) typ
    aRec <- genConstraintsExpr funTyp a
    bRec <- genConstraintsExpr bTyp b
    let metaData = TyExprMeta {
        exprPos = meta,
        exprTyp = typ
    }
    return $ App metaData aRec bRec

genConstraintsExpr typ (Var meta val) = do
    varTyp <- lookupSymbol val
    emitConstraint $ TyEq typ varTyp
    let metaData = TyExprMeta {
        exprPos = meta,
        exprTyp = typ
    }
    return $ Var metaData val

genConstraintsExpr typ (Construct meta constructor subexprs) = do
    constTyp <- lookupSymbol constructor

    tySubExprs <- forM subexprs $ \ sub -> do
        tv <- TyVar <$> getTyVar
        typedSubexpr <- genConstraintsExpr tv sub
        return (tv, typedSubexpr)

    -- curry type kinda
    let funType = foldr ((TyApp . TyApp (TyCon (Const "->"))) . fst ) typ tySubExprs

    emitConstraint $ TyEq funType constTyp

    let metaData = TyExprMeta {
        exprPos = meta,
        exprTyp = typ
    }
    return $ Construct metaData constructor $ map snd tySubExprs

--  | SrcStringLit (LitMeta a) String
genConstraintsExpr typ (SrcStringLit meta str) = do
    emitConstraint $ TyEq typ $ TyCon (Const "String")
    let metaData = TyExprMeta {
        exprPos = meta,
        exprTyp = typ
    }
    return $ SrcStringLit metaData str

--  | SrcIntLit (LitMeta a) Int
genConstraintsExpr typ (SrcIntLit meta int) = do
    emitConstraint $ TyEq typ $ TyCon (Const "Int")
    let metaData = TyExprMeta {
        exprPos = meta,
        exprTyp = typ
    }
    return $ SrcIntLit metaData int

--  | Lambda (LambdaMeta a) (Sym a) (Expr a)
genConstraintsExpr typ (Lambda meta boundSym body) =
    scope $ do
        symTyp <-  getTyVar
        addSymbol boundSym $ TyVar symTyp

        exprTyp <- TyVar <$> getTyVar
        constrainedBody <- genConstraintsExpr exprTyp body

        let funTyp = TyApp (TyApp (TyCon (Const "->")) (TyVar symTyp)) exprTyp
        emitConstraint $ TyEq typ funTyp

        let metaData = TyLambdaMeta {
            lambdaPos = Binding.position meta,
            lambdaTyp = typ,
            Typing.Pass.rec_bind = Binding.rec_bind meta,
            Typing.Pass.captures = Binding.captures meta
        }

        return $ Lambda metaData boundSym constrainedBody

--  | Case (CaseMeta a) (Expr a) [CaseBranch a]
genConstraintsExpr typ (Case meta scrut branches) = do 
    scrutTy <- TyVar <$> getTyVar
    tcScrut <- genConstraintsExpr scrutTy scrut 
    tcBranches <- forM branches $ \ (CaseBranch pat expr) -> 
        scope $ CaseBranch <$> genConstraintsPattern scrutTy pat <*> genConstraintsExpr typ expr 
    pure $ Case meta tcScrut tcBranches

--  | Extern (ExternMeta a) String -- we don't use it

genConstraints :: Toplevel Resolved -> TyEnv (Toplevel Typing)
genConstraints (Binding bindMeta name value) = do
    innerVar <- TyVar <$> getTyVar
    (vars, tyCheckExpr) <- tyScope $ genConstraintsExpr innerVar value
    let nameVar = foldl (\soFa cap -> TyForall (TyId cap) soFa) innerVar vars
    finalValue <- case Binding.boundTyp bindMeta of
        Nothing -> return nameVar
        Just typ -> do 
            typingTyp <- tcTypeDecl typ
            emitConstraint $ TyEq typingTyp nameVar
            return typingTyp
    addSymbol name finalValue
    let tyMeta = TyBindInfo {
        Typing.Pass.pos = Binding.pos bindMeta,
        Typing.Pass.id = Binding.id bindMeta,
        typ = finalValue
    }
    return $ Binding tyMeta name tyCheckExpr