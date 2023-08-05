{-# LANGUAGE OverloadedStrings #-}
module Tests where 
import Text.Parsec
import Parser
import Binding
import CodeGen
import GenRep

-- tests to be run in ghci


exampleParsed = let Right parsedTree = parse parseProgram "test" "main = (fn a -> a) \"Bitch\";" in parsedTree

exampleBound = bindProgram exampleParsed

exampleVirtGenerator = genProgram exampleBound
exampleVirt = runGenerator exampleVirtGenerator


testBindGenerator = 
    let
        Right parsedTree = parse parseProgram "test" "main = (fn a -> a) \"Bitch\";"
        boundTree = bindProgram parsedTree 
        virtGenerator = genProgram boundTree 
        virtGenerated = runGenerator virtGenerator
        mappedGenerated = runGenerator $ bindGeneratorCode Prelude.id (mapM_ emitCode) virtGenerator
    in 
        (virtGenerated, mappedGenerated)