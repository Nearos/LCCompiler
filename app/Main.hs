module Main where

import Parser
import Text.Parsec

import qualified Data.ByteString as BS
import Binding (resolveBindings, bindProgram)
import CodeGen (genProgram)
import PrintCode (Printable(printCode), printGenerated)
import GenRep (runGenerator, bindGeneratorCode, emitCode, mapGenerator, abiArg)
import System.Environment (getArgs)
import RegAlloc (regAllocNaive, regAlloc)
import System.Exit (exitFailure)
import Control.Monad (forM_, when)
import Tree (printAST, Toplevel(Binding), printProgram)
import RegAlloc.LiveRegister (findLiveRegs)
import Data.Bifunctor (Bifunctor(second))

data Settings = Settings {
    sourceFile :: String,
    outputFile :: String,
    dumpVirtualRegister :: Maybe String,
    dumpBound :: Maybe String
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
        getSettings [] = Settings "" "" Nothing Nothing
        getSettings ("--dVR":file:rest) = (getSettings rest) {dumpVirtualRegister = Just file}
        getSettings ("--dBound":file:rest) = (getSettings rest) {dumpBound = Just file}
        getSettings ("-o":file:rest) = (getSettings rest) {outputFile = file}
        getSettings (file:rest) =
            case getSettings rest of
                Settings _ "" d1 d2 -> Settings file (outputize file) d1 d2
                Settings _ opt d1 d2 -> Settings file opt d1 d2

        outputize file =
            let cName = reverse $ dropWhile (/= '.') $ reverse file
            in cName ++ ".arm64.s"



main :: IO ()
main = do
    settings <- readSettings
    exprSrc <- getSource settings
    -- TODO : whole program
    case parse parseProgram (sourceFile settings) exprSrc of
        Left perr -> do
            print perr
            exitFailure
        Right ast -> do
            putStrLn $ printProgram ast
            let bound = bindProgram ast
            case dumpBound settings of
                Nothing -> return ()
                Just file -> writeFile file $ printProgram bound
            let virtGenerator = genProgram bound
            let virtGenerated = runGenerator virtGenerator
            case dumpVirtualRegister settings of
                Nothing -> return ()
                Just file -> writeFile file $ printGenerated virtGenerated
            let allocatedRegisters = runGenerator $ bindGeneratorCode (fmap undefined . snd) regAlloc $ mapGenerator findLiveRegs virtGenerator
            saveResult settings $ printGenerated allocatedRegisters