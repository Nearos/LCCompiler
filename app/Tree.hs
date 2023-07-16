{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Tree where

type family BindMeta a
type family Sym a 
type family AppMeta a
type family LambdaMeta a 
type family VarMeta a
type family ExternMeta a
type family LitMeta a

data Expr a
    = Lambda (LambdaMeta a) (Sym a) (Expr a)
    | App (AppMeta a) (Expr a) (Expr a)
    | Var (VarMeta a) (Sym a)
    | SrcStringLit (LitMeta a) String
    | SrcIntLit (LitMeta a) Int
    | Extern (ExternMeta a) String

data Toplevel a 
    = Binding (BindMeta a) (Sym a) (Expr a)

class ShowablePass a where 
    passShow :: a -> String 


printAST ::
    ( ShowablePass (LambdaMeta a)
    , ShowablePass (VarMeta a)
    , ShowablePass (AppMeta a)
    , ShowablePass (Sym a)) 
    => Expr a -> String  
printAST (Lambda m s b) = "(\\" ++ passShow s ++ "." ++ printAST b ++ ")"
printAST (App m a b) = "(" ++ printAST a ++ " " ++ printAST b ++ ")"
printAST (Var m s) = passShow s 