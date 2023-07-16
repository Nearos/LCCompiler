module Main where

import Parser
import Text.Parsec

import qualified Data.ByteString as BS
import Binding (resolveBindings, bindProgram)
import CodeGen (genProgram)
import PrintCode (Printable(printCode), printGenerated)
import GenRep (runGenerator)
import System.Environment (getArgs)
import RegAlloc (regAllocNaive)
import System.Exit (exitFailure)
import Control.Monad (forM_)
import Tree (printAST, Toplevel(Binding))

data Settings = Settings {
    sourceFile :: String,
    outputFile :: String
    }   
    deriving Show

getSource :: Settings -> IO BS.ByteString
getSource settings 
    | sourceFile settings == "" = BS.getLine
    | otherwise = BS.readFile $ sourceFile settings

saveResult :: Settings -> String -> IO ()
saveResult settings
    | outputFile settings `elem` ["", "-"] = putStr
    | otherwise = writeFile (outputFile settings)

readSettings :: IO Settings
readSettings = getSettings <$> getArgs
    where 
        getSettings [] = Settings "" ""
        getSettings ("-o":file:rest) = (getSettings rest) {outputFile = file}
        getSettings (file:rest) = 
            case getSettings rest of 
                Settings _ "" -> Settings file $ outputize file 
                Settings _ opt -> Settings file opt       

        outputize file = 
            let cName = reverse $ dropWhile (/= '.') $ reverse file
            in cName ++ ".arm64.s"



main :: IO ()
main = do 
    settings <- readSettings
    exprSrc <- getSource settings
    -- TODO : whole program
    case parse parseProgram "stdin" exprSrc of 
        Left perr -> do 
            print perr
            exitFailure
        Right ast -> do 
            let bound = bindProgram ast 
            let virtGenerated = runGenerator $ genProgram bound 
            let allocatedRegisters = runGenerator $ regAllocNaive [] virtGenerated
            saveResult settings $ printGenerated allocatedRegisters

