{-# LANGUAGE FlexibleInstances #-}
module PrintCode where 

import GenRep
import Data.List (intercalate)
class Printable a where 
    printCode :: a -> String 

instance Printable reg => Printable (RegArg reg) where
    printCode (In reg) = printCode reg
    printCode (Out reg) = printCode reg
    printCode (InOut reg) = printCode reg

instance Printable reg => Printable (FlexibleArg reg) where 
    printCode (ImmInt l) = show l
    printCode (ImmLabel l) = l 
    printCode (ImmString lit) = "\"" ++ lit ++ "\""
    printCode (ImmChar lit) = "\'" ++ [lit] ++ "\'"
    printCode (DerefReg reg) = "[" ++ printCode reg ++ "]" 
    printCode (LabelAddr str) = "="++str 
    printCode (DerefExpr reg n) = "[" ++ printCode reg ++", " ++ show n ++ "]"
    printCode (DerefPre reg n) = "[" ++ printCode reg ++", " ++ show n ++ "]!"
    printCode (DerefPost reg n) = "[" ++ printCode reg ++"], " ++ show n

instance Printable reg => Printable (ARM64 reg) where 
    printCode (InstFlex mnem regargs  val) 
        = "    " ++ mnem ++ " " ++  concatMap ((++ ", ") . printCode) regargs ++ printCode val
    
    printCode (InstRegs mnem regargs) 
        = "    " ++ mnem ++ " " ++ intercalate ", " (map printCode regargs) 

    printCode (InstHiddenFlex mnem _ regargs  val) 
        = "    " ++ mnem ++ " " ++ concatMap ((++ ", ") . printCode) regargs ++ printCode val

    printCode (InstHiddenReg mnem _ regargs) 
        = "    " ++ mnem ++ " " ++ intercalate ", " (map printCode regargs) 

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