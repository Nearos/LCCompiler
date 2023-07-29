{-# LANGUAGE LambdaCase #-}
module RegAlloc where 

import GenRep
import Control.Monad.Trans.Accum (add)
import Control.Monad (MonadPlus(mplus), forM, foldM, forM_)
import Data.IntMap (insertWith')
import Text.Read (Lexeme(String))
import Data.Maybe (fromMaybe, fromJust)
import Data.ByteString (cons)
import PrintCode (Printable(printCode))

import RegAlloc.InstructionTransactions


regAllocNaive :: [(Int, String)] -> [ARM64 VirtualRegister] -> Generator (ARM64 String) ()
regAllocNaive _ [] = return ()
regAllocNaive ass (inst:insts) = do 
    ass <- foldM extendLabelAssignment ass $  consumes ++ produces       
    forM_ consumes $ \case 
        Virt n -> do 
            emitCode $ InstFlex "ldr" [Out "x2"] $ LabelAddr $ fromJust $ lookup n ass 
            emitCode $ InstFlex "ldr" [Out $ findHardRegister n] $ DerefReg "x2"
        _ -> return ()
    emitCode $ replaceRegister <$> inst 
    forM_ produces $ \case 
        Virt n -> do 
            emitCode $ InstFlex "ldr" [In "x2"] $ LabelAddr $ fromJust $ lookup n ass 
            emitCode $ InstFlex "str" [In $ findHardRegister n] $ DerefReg "x2"
        _ -> return ()
    regAllocNaive ass insts
    where 
        RegisterTransaction consumes produces = instructionTransaction inst
        allRegisters = consumes ++ produces

        registerAssignment 
            = zip ((consumes ++ produces) 
            >>= \case 
                Virt n -> [n]
                _ -> []) [3..]

        findHardRegister n = 
            let hardRegisterNumber = lookup n registerAssignment 
            in "x" ++ show (fromJust hardRegisterNumber)

        replaceRegister (Virt n) = findHardRegister n 
        replaceRegister other = printCode other

        extendLabelAssignment :: [(Int, String)] -> VirtualRegister -> Generator (ARM64 String) [(Int, String)]
        extendLabelAssignment ass (Virt n)=
            case lookup n ass of 
                Just {} -> return ass 
                Nothing -> do 
                    label <- getLabel "register_spill"
                    emitData $ DefLabel label
                    emitData $ Pseudo ".quad" $ ImmInt 0
                    return $ (n, label) : ass

        extendLabelAssignment ass _ = return ass
