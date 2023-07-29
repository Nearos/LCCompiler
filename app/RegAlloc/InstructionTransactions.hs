module RegAlloc.InstructionTransactions where 

import GenRep

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