module Typing.ConstraintSolve where

import Typing.Environment
import Typing.Constraint
import Typing.Pass

solveConstraint :: Constraint Typing -> TyEnv (Either String String)
solveConstraint (TyEq ty1 ty2) = undefined


solveConstraint (TyInst sym1 sym2) = undefined -- mirror all constraints containing sym2 with sym1