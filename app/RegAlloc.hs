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

data RegisterTransaction reg = RegisterTransaction [reg] [reg]

instance Semigroup (RegisterTransaction reg) where 
    (<>) (RegisterTransaction a b) (RegisterTransaction c d) 
        = RegisterTransaction (a <> c) (b <> d)

instance Monoid (RegisterTransaction reg) where 
    mempty = RegisterTransaction [] []
    
flexArgTransaction :: FlexibleArg reg -> RegisterTransaction reg
flexArgTransaction (DerefReg reg) = RegisterTransaction [reg] [] 
flexArgTransaction (LabelAddr _) = mempty
flexArgTransaction (DerefExpr reg _) = RegisterTransaction [reg] []
flexArgTransaction (DerefPost reg _) = RegisterTransaction [reg] [reg]
flexArgTransaction (DerefPre reg _ ) = RegisterTransaction [reg] [reg]
flexArgTransaction (ImmInt _) = mempty
flexArgTransaction (ImmLabel _) = mempty
flexArgTransaction (ImmString _) = mempty
flexArgTransaction (ImmChar _) = mempty

regArgTransaction :: RegArg reg -> RegisterTransaction reg
regArgTransaction (In reg) = RegisterTransaction [reg] []
regArgTransaction (Out reg) = RegisterTransaction [] [reg]
regArgTransaction (InOut reg) = RegisterTransaction [reg] [reg]

instructionTransaction :: ARM64 reg -> RegisterTransaction reg
instructionTransaction (InstFlex _ regs flex) = foldMap regArgTransaction regs <> flexArgTransaction flex
instructionTransaction (InstRegs _ regs) = foldMap regArgTransaction regs 
instructionTransaction (InstHiddenFlex _ regs1 regs2 flex) = foldMap regArgTransaction (regs1 <> regs2) <> flexArgTransaction flex
instructionTransaction (InstHiddenReg _ regs1 regs2) = foldMap regArgTransaction (regs1 <> regs2) 
instructionTransaction _ = mempty


regAllocNaive :: [(Int, String)] -> [ARM64 VirtualRegister] -> Generator String ()
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

        extendLabelAssignment :: [(Int, String)] -> VirtualRegister -> Generator String [(Int, String)]
        extendLabelAssignment ass (Virt n)=
            case lookup n ass of 
                Just {} -> return ass 
                Nothing -> do 
                    label <- getLabel "register_spill"
                    emitData $ DefLabel label
                    emitData $ Pseudo ".quad" $ ImmInt 0
                    return $ (n, label) : ass

        extendLabelAssignment ass _ = return ass
