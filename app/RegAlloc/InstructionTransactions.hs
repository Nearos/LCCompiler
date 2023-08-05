module RegAlloc.InstructionTransactions where 

import GenRep
import qualified Data.Set as S

{-
    This file contains code to extract information from instructions to be used in register allocation. So far:
        - RegisterTransaction
            Registers that this instruction depends on and produces
        - InstructionControl
            Which code runs after this instruction. Does it call a function, jump to a label or just run inline
-}

data RegisterTransaction reg = RegisterTransaction (S.Set reg) (S.Set reg)

instance Ord reg => Semigroup (RegisterTransaction reg) where 
    (<>) (RegisterTransaction a b) (RegisterTransaction c d) 
        = RegisterTransaction (a <> c) (b <> d)

instance Ord reg =>  Monoid (RegisterTransaction reg) where 
    mempty = RegisterTransaction mempty mempty
    
flexArgTransaction :: Ord reg => FlexibleArg reg -> RegisterTransaction reg
flexArgTransaction (DerefReg reg) = RegisterTransaction (S.singleton reg) mempty
flexArgTransaction (LabelAddr _) = mempty
flexArgTransaction (DerefExpr reg _) = RegisterTransaction (S.singleton reg) mempty
flexArgTransaction (DerefPost reg _) = RegisterTransaction (S.singleton reg) (S.singleton reg)
flexArgTransaction (DerefPre reg _ ) = RegisterTransaction (S.singleton reg) (S.singleton reg)
flexArgTransaction (ImmInt _) = mempty
flexArgTransaction (ImmLabel _) = mempty
flexArgTransaction (ImmString _) = mempty
flexArgTransaction (ImmChar _) = mempty

regArgTransaction :: Ord reg => RegArg reg -> RegisterTransaction reg
regArgTransaction (In reg) = RegisterTransaction (S.singleton reg) mempty
regArgTransaction (Out reg) = RegisterTransaction mempty (S.singleton reg)
regArgTransaction (InOut reg) = RegisterTransaction (S.singleton reg) (S.singleton reg)

instructionTransaction :: Ord reg => ARM64 reg -> RegisterTransaction reg
instructionTransaction (InstFlex _ regs flex) = foldMap regArgTransaction regs <> flexArgTransaction flex
instructionTransaction (InstRegs _ regs) = foldMap regArgTransaction regs 
instructionTransaction (InstHiddenFlex _ regs1 regs2 flex) = foldMap regArgTransaction (regs1 <> regs2) <> flexArgTransaction flex
instructionTransaction (InstHiddenReg _ regs1 regs2) = foldMap regArgTransaction (regs1 <> regs2) 
instructionTransaction _ = mempty

data InstructionControl  
    = CondJump String 
    | Jump String 
    | Funcall 
    | Inline
    deriving Eq

instructionJump :: ARM64 reg -> InstructionControl
instructionJump (InstHiddenReg "blr" _ _) = Funcall
instructionJump (InstHiddenFlex "bl" _ _ _) = Funcall
instructionJump (InstFlex "beq" _ (ImmLabel label)) = CondJump label
instructionJump _ = Inline