{-# LANGUAGE DeriveFunctor #-}
module GenRep where 
import Control.Monad.Trans.State (State, modify, gets, execState)
import Binding (Binding)

-- Assembly

data Value 
    = IntLit Int
    | Label String
    | StringLit String 
    | CharLit Char

data Address reg
    = RegAddr reg -- [x0]
    | LabelAddr String -- =printf
    | ExprAddr reg Int -- [x0, 8]
    | Preorder reg Int -- [x0, 8]! <=> adds 8 to x0 then peforms op from x0
    | Postorder reg Int -- [x0], 8 <=> performs op from x0 then adds 8 to it
    deriving Functor
 
data ARM64 reg 
    = Immediate String reg reg Value -- addi r1, r2, 5
    | Register String reg reg reg  --add r1, r2, r3
    | OneRegister String reg 
    | TwoRegister String reg reg
    | OneImmediate String Value
    | TwoImmediate String reg Value
    | Memory String reg (Address reg)
    | Comment String -- #comment
    | DefLabel String -- label:
    | Pseudo String Value -- .dword 5
    | PseudoZero String
    deriving Functor

data GenerationContext reg
    = GenerationContext {
        pushedCode :: [[ARM64 reg]],
        code :: [[ARM64 reg]],
        constData :: [ARM64 reg],
        labelId :: Int,
        registerId :: Int
    }
type Generator reg = State (GenerationContext reg)

-- Push and pop code:
--  saves the current positon in generation to generate a new section, which is placed at the top when code is popped again

pushCode :: Generator a () 
pushCode = modify $ \ctx -> ctx { code = [] : code ctx, pushedCode = head (code ctx) : pushedCode ctx}

popCode :: Generator a () 
popCode 
    = modify $ 
        \ctx -> 
            case pushedCode ctx of 
                [] -> ctx 
                x : xs -> ctx {code = x : code ctx, pushedCode = xs}

emitCode :: ARM64 a -> Generator a ()
emitCode line = modify $ 
    \ ctx -> case code ctx of 
        first : rest -> ctx { code = (line : first) : rest}

emitData :: ARM64 a -> Generator a ()
emitData line = modify $ \ ctx -> ctx { constData = line : constData ctx}

getLabel :: String -> Generator a String 
getLabel name = do 
    intLabel <- gets labelId
    modify $ \ctx -> ctx { labelId = intLabel + 1}
    return $ name ++ "_" ++ show intLabel

data VirtualRegister 
    = X Int 
    | W Int
    | XZR 
    | WZR
    | SP
    | Virt Int
    | Virt32 Int

getVReg :: Generator VirtualRegister VirtualRegister
getVReg = do 
    intLabel <- gets registerId
    modify $ \ctx -> ctx { registerId = intLabel + 1}
    return $ Virt intLabel

--useful registers
zero :: VirtualRegister
zero = XZR
sp :: VirtualRegister
sp = SP

-- Structures 

newtype Struct = Struct [(Binding, Int)] -- [(fieldName, fieldSize)]

sizeOf :: Struct -> Int 
sizeOf (Struct s) = sum $ map snd s

offsetOf :: Struct -> Binding -> Int 
offsetOf (Struct s) field = go s 0
    where 
        go [] acc =  undefined
        go ((f, size):rest) acc
            | f == field = acc 
            | otherwise = go rest (acc + size)

-- Closures

abiCtx :: VirtualRegister
abiCtx = X 0
abiArg :: VirtualRegister
abiArg = X 1
abiRet :: VirtualRegister
abiRet = abiArg

closure :: Struct 
closure 
    = Struct [
        (("fn", -1), 8),
        (("context", -1), 8)
    ]

callClosure :: VirtualRegister -> Generator VirtualRegister VirtualRegister
callClosure cl = do 
    emitCode $ Comment "load closure context"
    emitCode $ Memory "ldr" abiCtx $ ExprAddr cl $ offsetOf closure ("context", -1)
    funAddr <- getVReg
    emitCode $ Comment "load function address"
    emitCode $ Memory "ldr" funAddr $ ExprAddr cl $ offsetOf closure ("fn", -1)

    emitCode $ Comment "branch to fn"
    emitCode $ OneRegister "blr" funAddr
    resultReg <- getVReg
    emitCode $ Comment "move result"
    emitCode $ TwoRegister "mov" resultReg abiRet
    return resultReg

runGenerator :: Generator a b -> [ARM64 a]
runGenerator gen = 
    [PseudoZero "", PseudoZero ".text"] 
    ++ reverse (concatMap reverse $ code finalState) 

    ++ [PseudoZero "", PseudoZero ".data"] 
    ++ reverse (constData finalState)
    where 
        finalState = execState gen initialState 
        initialState = GenerationContext {
            pushedCode = [],
            code = [[]],
            constData = [],
            labelId = 0,
            registerId = 0
        }
