{-# LANGUAGE TypeFamilies, StandaloneDeriving, UndecidableInstances, FlexibleContexts #-}
module Typing.Typ where 

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