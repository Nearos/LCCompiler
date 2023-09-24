{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
module Typing.Constraint where 

import Typing.Typ(Typ, TySym, SymbolFunctor(..))
import Tree (ShowablePass (passShow))
import Data.List (intercalate)

data Constraint pass
    = TyEq (Typ pass) (Typ pass)
    | TyInst (TySym pass) (TySym pass)

deriving instance (Show (Typ pass), Show (TySym pass)) => Show (Constraint pass)

instance (ShowablePass (Typ pass), ShowablePass (TySym pass)) => ShowablePass (Constraint pass) where
    passShow (TyEq a b) = passShow a ++ "  ~~  " ++ passShow b
    passShow (TyInst a b) = passShow a ++ "  _is_  " ++ passShow b

showConstraints :: (ShowablePass (Typ pass), ShowablePass (TySym pass)) => [Constraint pass] -> String 
showConstraints = intercalate "\n" . map passShow

instance SymbolFunctor Constraint where
    sfmap f (TyEq a b) = TyEq (sfmap f a) (sfmap f b)
    sfmap f (TyInst a b) = TyInst (f a) (f b)