{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Typing.Environment where 

import Typing.Constraint
import Typing.Typ (TySym, Typ (TyVar))
import Typing.Pass

import Tree
import Binding (ResolvedSymbol, Binding)

import qualified Data.Map as M

import Optics (makeLenses, (^.), view, ToReadOnly (getting))
import Optics.State.Operators
import Control.Monad.Trans.State (State, get, gets, put, modify, withState, evalState)

import Text.Parsec(SourcePos)

-- Type checking pass



--- 

data TypingState 
    = TypingState {
        _currentTyVarId :: Int,
        _outConstraints :: [Constraint Typing],
        _nameScope :: M.Map (Sym Typing) (Typ Typing)
    }

initialTypingState = TypingState {
    _currentTyVarId = 0,
    _outConstraints = [],
    _nameScope = M.empty
}

makeLenses ''TypingState

type TyEnv = State TypingState

-- defining operations on the monad

getTyVar :: TyEnv TyId
getTyVar = fmap TyId $ currentTyVarId <<%= (+1)

scope :: TyEnv a -> TyEnv a 
scope inner = do 
    previousNameScope <- gets $ view nameScope
    ret <- inner 
    nameScope .= previousNameScope
    return ret

tyScope :: TyEnv a -> TyEnv ([Int], a) 
tyScope inner = do 
    startingId <- gets $ view currentTyVarId
    ret <- inner 
    finalId <- gets $ view currentTyVarId 
    pure ([startingId .. finalId], ret)

addSymbol :: Sym Typing -> Typ Typing -> TyEnv ()
addSymbol name typ = nameScope %= M.insert name typ

lookupSymbol :: Sym Typing -> TyEnv (Typ Typing) 
lookupSymbol sym = do 
    value <- gets $ M.lookup sym . view nameScope
    case value of 
        Just ty -> return ty
        Nothing -> do 
            tv <- getTyVar
            let typ = TyVar tv
            addSymbol sym typ 
            return typ

emitConstraint :: Constraint Typing -> TyEnv ()
emitConstraint ct = do 
    outConstraints %= (ct :)

-- operations for constraint solving

