{-# LANGUAGE TypeFamilies, StandaloneDeriving, UndecidableInstances, FlexibleContexts #-}
module Typing.Typ where 
import Tree (ShowablePass (passShow))

type family TySym a

data Typ pass
    = TyCon (TySym pass) 
    | TyVar (TySym pass) 
    | TyForall (TySym pass) (Typ pass)
    | TyApp (Typ pass) (Typ pass)

deriving instance Eq (TySym pass) => Eq (Typ pass)
deriving instance Show (TySym pass) => Show (Typ pass)


arity :: (Eq (TySym pass), Num a) => TySym pass -> Typ pass -> a
arity arrowSym (TyApp (TyApp (TyCon conSym) a) b) 
    | conSym == arrowSym = 1 + arity arrowSym b
    | otherwise = 0 
arity _ _ = 0

instance ShowablePass (TySym pass) => ShowablePass (Typ pass) where
    passShow (TyApp (TyApp (TyCon op) a) b) 
        | passShow op == "->" = "( " ++ passShow a ++ " -> " ++ passShow b ++ " )"
    passShow (TyCon sym) = passShow sym 
    passShow (TyVar sym) = passShow sym 
    passShow (TyForall sym typ) = "forall " ++ passShow sym ++ " . " ++ passShow typ
    passShow (TyApp a b) = "(" ++ passShow a ++ " " ++ passShow b ++ ")"


class SymbolFunctor f where 
    sfmap :: (TySym a -> TySym b) -> f a -> f b

instance SymbolFunctor Typ where 
    sfmap f (TyCon a) = TyCon $ f a
    sfmap f (TyVar a) = TyVar $ f a
    sfmap f (TyForall a b) = TyForall (f a) (sfmap f b)
    sfmap f (TyApp a b) = TyApp (sfmap f a) (sfmap f b)