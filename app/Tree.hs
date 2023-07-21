{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Tree where
import Data.List (intercalate)

type family BindMeta a
type family Sym a 
type family Const a
type family AppMeta a
type family LambdaMeta a 
type family VarMeta a
type family ExternMeta a
type family LitMeta a
type family ConsMeta a

data Expr a
    = Lambda (LambdaMeta a) (Sym a) (Expr a)
    | App (AppMeta a) (Expr a) (Expr a)
    | Var (VarMeta a) (Sym a)
    | Construct (ConsMeta a) (Const a) [Expr a]
    | SrcStringLit (LitMeta a) String
    | SrcIntLit (LitMeta a) Int
    | Extern (ExternMeta a) String

data Toplevel a 
    = Binding (BindMeta a) (Sym a) (Expr a)
    | ConstDef (BindMeta a) (Const a) Int

class ShowablePass a where 
    passShow :: a -> String 


printAST ::
    ( ShowablePass (BindMeta a)
    , ShowablePass (Sym a )
    , ShowablePass (Const a)
    , ShowablePass (AppMeta a)
    , ShowablePass (LambdaMeta a)
    , ShowablePass (VarMeta a)
    , ShowablePass (ExternMeta a)
    , ShowablePass (LitMeta a)
    , ShowablePass (ConsMeta a))
    => Expr a -> String  
printAST (Lambda m s b) = "([" ++ passShow m ++ "]\\" ++ passShow s ++ "." ++ printAST b ++ ")"
printAST (App m a b) = "(" ++ printAST a ++ " " ++ printAST b ++ ")"
printAST (Var m s) = passShow s 
printAST (Construct m c subs) = passShow c ++ "{" ++ intercalate ", " (map printAST subs) ++ "}"
printAST (SrcStringLit m val) = "\"" ++ val ++ "\"" 

printToplevel (Binding meta sym expr) = passShow sym ++ " = " ++ printAST expr ++ ";"
printToplevel (ConstDef meat sym arity) = passShow sym ++ " : " ++ show arity ++ ";"

printProgram :: 
    ( ShowablePass (BindMeta a)
    , ShowablePass (Sym a )
    , ShowablePass (Const a)
    , ShowablePass (AppMeta a)
    , ShowablePass (LambdaMeta a)
    , ShowablePass (VarMeta a)
    , ShowablePass (ExternMeta a)
    , ShowablePass (LitMeta a)
    , ShowablePass (ConsMeta a))
    => [Toplevel a] -> [Char]
printProgram = intercalate "\n" . map printToplevel 