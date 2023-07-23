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
    
addressTransaction :: Address reg -> RegisterTransaction reg
addressTransaction (RegAddr reg) = RegisterTransaction [reg] []
addressTransaction (ExprAddr reg _) = RegisterTransaction [reg] []
addressTransaction (Preorder reg _) = RegisterTransaction [reg] [reg]
addressTransaction (Postorder reg _) = RegisterTransaction [reg] [reg]
addressTransaction (LabelAddr _) = RegisterTransaction [] []

instructionTransaction :: ARM64 VirtualRegister -> RegisterTransaction VirtualRegister

instructionTransaction (Immediate _ r1 r2 _) = RegisterTransaction [r2] [r1]
instructionTransaction (Register _ rr r1 r2) = RegisterTransaction [r1, r2] [rr]
instructionTransaction (OneRegister mnem r1)
    | mnem == "blr" = RegisterTransaction [r1, abiCtx, abiArg] [abiRet] -- function call instructionTransaction argument and context

instructionTransaction (OneImmediate mnem _) 
    | mnem == "bl" = RegisterTransaction [abiCtx, abiArg] [abiRet] --maybe no context but it's ok
    | otherwise = mempty

instructionTransaction (TwoImmediate "cmp" reg _) = RegisterTransaction [reg] []
instructionTransaction (TwoImmediate _ reg _) = RegisterTransaction [] [reg]
instructionTransaction (Memory mnem reg addr) = opTransaction <> addressTransaction addr
    where
        opTransaction 
            = case mnem of 
                'l':'d':_ -> RegisterTransaction [] [reg]
                's':'t':_ -> RegisterTransaction [reg] [] 

instructionTransaction (TwoRegister "mov" r1 r2) = RegisterTransaction [r2] [r1]

instructionTransaction (Comment {}) = mempty
instructionTransaction (DefLabel {}) = mempty
instructionTransaction (Pseudo {}) = mempty
instructionTransaction (PseudoZero {}) = mempty

regAllocNaive :: [(Int, String)] -> [ARM64 VirtualRegister] -> Generator String ()
regAllocNaive _ [] = return ()
regAllocNaive ass (inst:insts) = do 
    ass <- foldM extendLabelAssignment ass $  consumes ++ produces       
    forM_ consumes $ \case 
        Virt n -> do 
            emitCode $ Memory "ldr" "x2" $ LabelAddr $ fromJust $ lookup n ass 
            emitCode $ Memory "ldr" (findHardRegister n) $ RegAddr "x2"
        _ -> return ()
    emitCode $ replaceRegister <$> inst 
    forM_ produces $ \case 
        Virt n -> do 
            emitCode $ Memory "ldr" "x2" $ LabelAddr $ fromJust $ lookup n ass 
            emitCode $ Memory "str" (findHardRegister n) $ RegAddr "x2"
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
                    emitData $ Pseudo ".quad" $ IntLit 0
                    return $ (n, label) : ass

        extendLabelAssignment ass _ = return ass
