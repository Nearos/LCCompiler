{-# LANGUAGE OverloadedStrings #-}
module Tests where 
import Text.Parsec
import Parser
import Binding
import CodeGen
import GenRep
import Typing.ConstraintGen (genConstraints)
import Control.Monad.Trans.State
import Typing.Environment (initialTypingState, TypingState (_outConstraints), outConstraints)
import Typing.Pass
import Optics ((^.))
import Typing.Constraint (Constraint)

-- tests to be run in ghci


exampleParsed = let Right parsedTree = parse parseProgram "test" "funA = fn b -> (fn c -> b); main = (fn a -> funA) \"Bitch\" 3 4;" in parsedTree

exampleBound = bindProgram exampleParsed

exampleVirtGenerator = genProgram exampleBound
exampleVirt = runGenerator exampleVirtGenerator

exampleConstrained = runState (mapM genConstraints exampleBound) initialTypingState

exampleConstraints :: [Constraint Typing]
exampleConstraints = snd exampleConstrained ^. outConstraints

exampleTyped = fst exampleConstrained

testBindGenerator = 
    let
        Right parsedTree = parse parseProgram "test" "main = (fn a -> a) \"Bitch\";"
        boundTree = bindProgram parsedTree 
        virtGenerator = genProgram boundTree 
        virtGenerated = runGenerator virtGenerator
        mappedGenerated = runGenerator $ bindGeneratorCode Prelude.id (mapM_ emitCode) virtGenerator
    in 
        (virtGenerated, mappedGenerated)