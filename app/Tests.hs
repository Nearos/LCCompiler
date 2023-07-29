{-# LANGUAGE OverloadedStrings #-}
module Tests where 
import Text.Parsec
import Parser
import Binding
import CodeGen
import GenRep

-- tests to be run in ghci

testBindGenerator = 
    let
        Right parsedTree = parse parseProgram "test" "main = (fn a -> a) \"Bitch\";"
        boundTree = bindProgram parsedTree 
        virtGenerator = genProgram boundTree 
        virtGenerated = runGenerator virtGenerator
        mappedGenerated = runGenerator $ bindGenerator (mapM_ emitCode) virtGenerator
    in 
        (virtGenerated, mappedGenerated)