{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Typing.Pass where

import Tree 
import Typing.Typ

import Binding

import Text.Parsec

data Typing 

data TyId 
    = TyId Int
    | Const String
    deriving (Eq, Ord, Show)

instance ShowablePass TyId where
    passShow (Const str) = str
    passShow (TyId i) = "'" ++ show i 

type instance Sym Typing = ResolvedSymbol
type instance Const Typing = ResolvedSymbol
type instance TySym Typing = TyId
type instance ExternMeta Typing = SourcePos
type instance CaseMeta Typing = SourcePos
type instance BindMeta Typing = TyBindInfo Typing

data TyBindInfo pass = TyBindInfo {
    pos :: SourcePos,
    id :: Int, 
    typ :: Typ pass
}

instance ShowablePass (Typ pass) => ShowablePass (TyBindInfo pass) where 
    passShow (TyBindInfo pos id typ) = 
        "@" ++ show pos ++ " #" ++ show id ++ " :" ++ passShow typ 

type instance AppMeta Typing = TyExprMeta Typing
type instance VarMeta Typing = TyExprMeta Typing
type instance ConsMeta Typing = TyExprMeta Typing
type instance LitMeta Typing = TyExprMeta Typing

data TyExprMeta pass = TyExprMeta {
    exprPos :: SourcePos,
    exprTyp :: Typ pass
}
instance ShowablePass (Typ pass) => ShowablePass (TyExprMeta pass) where 
    passShow (TyExprMeta _ typ) = " : " ++ passShow typ

type instance LambdaMeta Typing = TyLambdaMeta Typing

data TyLambdaMeta pass = TyLambdaMeta {
    lambdaPos :: SourcePos,
    lambdaTyp :: Typ pass,
    captures :: [Binding],
    rec_bind :: Maybe Binding
}

instance ShowablePass (Typ pass) => ShowablePass (TyLambdaMeta pass) where 
    passShow (TyLambdaMeta _ typ _ _) = " : " ++ passShow typ 
