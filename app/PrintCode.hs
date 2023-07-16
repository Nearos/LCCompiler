{-# LANGUAGE FlexibleInstances #-}
module PrintCode where 

import GenRep
class Printable a where 
    printCode :: a -> String 

instance Printable Value where 
    printCode (IntLit l) = show l
    printCode (Label l) = l 
    printCode (StringLit lit) = "\"" ++ lit ++ "\""
    printCode (CharLit lit) = "\'" ++ [lit] ++ "\'"


instance Printable reg => Printable (Address reg) where 
    printCode (RegAddr reg) = "[" ++ printCode reg ++ "]" 
    printCode (LabelAddr str) = "="++str 
    printCode (ExprAddr reg n) = "[" ++ printCode reg ++", " ++ show n ++ "]"
    printCode (Preorder reg n) = "[" ++ printCode reg ++", " ++ show n ++ "]!"
    printCode (Postorder reg n) = "[" ++ printCode reg ++"], " ++ show n

instance Printable reg => Printable (ARM64 reg) where 
    printCode (Immediate mnem r1 r2 val) 
        = "    " ++ mnem ++ " " ++ printCode r1 ++", " ++ printCode r2 ++ ", " ++ printCode val

    printCode (Register mnem r1 r2 r3) 
        = "    " ++ mnem ++ " " ++ printCode r1 ++", " ++ printCode r2 ++ ", " ++ printCode r3 
    
    printCode (OneImmediate mnem val) 
        = "    " ++ mnem ++ " " ++ printCode val 

    printCode (TwoImmediate mnem r1 val) 
        = "    " ++ mnem ++ " " ++ printCode r1 ++", " ++ printCode val 

    printCode (OneRegister mnem r1) 
        = "    " ++ mnem ++ " " ++ printCode r1

    printCode (TwoRegister mnem r1 r2) 
        = "    " ++ mnem ++ " " ++ printCode r1 ++", " ++ printCode r2
    
    printCode (Memory mnem r1 val) 
        = "    " ++ mnem ++ " " ++ printCode r1 ++", " ++ printCode val 

    printCode (Comment str) = "\n    //" ++ str 

    printCode (DefLabel str) = str ++ ": "

    printCode (Pseudo str val ) = "    " ++ str ++ " " ++ printCode val

    printCode (PseudoZero str) = str

instance Printable VirtualRegister where 
    printCode (W n) = "w" ++ show n 
    printCode (X n) = "x" ++ show n 
    printCode XZR = "xzr"
    printCode WZR = "wzr"
    printCode SP = "sp"
    printCode (Virt n) = "v" ++ show n 
    printCode (Virt32 n) = "vw" ++ show n 

instance Printable [Char] where 
    printCode = id

printGenerated :: Printable reg => [ARM64 reg] -> String
printGenerated = foldr ((\a b -> a ++ "\n" ++ b) . printCode) " "