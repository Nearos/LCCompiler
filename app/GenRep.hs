{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
module GenRep where
import Control.Monad.Trans.State (State, modify, gets, execState, runState, put)
import Binding (Binding)

-- Assembly

data FlexibleArg reg
    = DerefReg reg -- [x0]
    | LabelAddr String -- =printf
    | DerefExpr reg Int -- [x0, 8]
    | DerefPost reg Int -- [x0, 8]! <=> adds 8 to x0 then peforms op from x0
    | DerefPre reg Int -- [x0], 8 <=> performs op from x0 then adds 8 to it
    | ImmInt Int -- 5
    | ImmLabel String -- printf
    | ImmString String -- "Hello!"
    | ImmChar Char -- 'c'\
    deriving Functor

data RegArg reg
    = In reg
    | Out reg
    | InOut reg
    deriving Functor

data ARM64 reg
    = InstFlex String [RegArg reg] (FlexibleArg reg)
    | InstRegs String [RegArg reg]
    | InstHiddenFlex String [RegArg reg] [RegArg reg] (FlexibleArg reg)
    | InstHiddenReg String [RegArg reg] [RegArg reg]
    | Comment String -- #comment
    | DefLabel String -- label:
    | Pseudo String (FlexibleArg reg) -- .dword 5
    | PseudoZero String -- .text
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
pushCode = modify $ \ctx -> ctx { code = [] : tail (code ctx), pushedCode = head (code ctx) : pushedCode ctx}

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

-- Abi

abiCtx :: VirtualRegister
abiCtx = X 0
abiArg :: VirtualRegister
abiArg = X 1
abiRet :: VirtualRegister
abiRet = abiArg

funcallFlex :: String -> [RegArg VirtualRegister] -> FlexibleArg VirtualRegister -> ARM64 VirtualRegister
funcallFlex op = InstHiddenFlex op [In abiCtx, In abiArg, Out abiRet]

funcallRegs :: String -> [RegArg VirtualRegister] -> ARM64 VirtualRegister
funcallRegs op = InstHiddenReg op [In abiCtx, In abiArg, Out abiRet]
-- Closures

closure :: Struct
closure
    = Struct [
        (("fn", -1), 8),
        (("context", -1), 8)
    ]

callClosure :: VirtualRegister -> Generator VirtualRegister VirtualRegister
callClosure cl = do
    emitCode $ Comment "load closure context"
    emitCode $ InstFlex "ldr" [Out abiCtx] $ DerefExpr cl $ offsetOf closure ("context", -1)
    funAddr <- getVReg
    emitCode $ Comment "load function address"
    emitCode $ InstFlex "ldr" [Out funAddr] $ DerefExpr cl $ offsetOf closure ("fn", -1)

    emitCode $ Comment "branch to fn"
    emitCode $ funcallRegs "blr" [In funAddr]
    resultReg <- getVReg
    emitCode $ Comment "move result"
    emitCode $ InstRegs "mov" [Out resultReg, In abiRet]
    return resultReg

runGenerator :: Generator a b -> [ARM64 a]
runGenerator gen =
    [PseudoZero "", PseudoZero ".text"]
    ++ reverse (concat $ code finalState)

    ++ [PseudoZero "", PseudoZero ".data"]
    ++ reverse (constData finalState)
    where
        finalState = execState gen initialState

initialState :: GenerationContext reg
initialState = GenerationContext {
    pushedCode = [],
    code = [[]],
    constData = [],
    labelId = 0,
    registerId = 0
}

-- generator transformations

mapGenerator ::([ARM64 reg] -> [ARM64 reg2]) ->  Generator reg a ->  Generator reg2 a
mapGenerator trans generator = value <$ put finalState
    where
        (   value,
            GenerationContext {
                pushedCode,
                code,
                constData,
                labelId,
                registerId
            }
            ) = runState generator initialState

        finalState = GenerationContext {
            pushedCode = map trans pushedCode,
            code = map trans code,
            constData = trans constData,
            labelId,
            registerId
        }

-- kind of a monad in its first type arg
bindGenerator :: ([ARM64 reg] -> Generator reg2 ()) -> Generator reg a -> Generator reg2 ()
bindGenerator trans gen = boundGenerator
    where 
        (   _,
            GenerationContext {
                pushedCode,
                code = oldCode,
                constData,
                labelId,
                registerId
            }
            ) = runState gen initialState

        [] = pushedCode

        newInitialContext 
            = GenerationContext {
                pushedCode = [],
                code = [[]],
                constData = [],
                labelId,
                registerId
            }
        
        bindCode [] = return () 
        bindCode (section : sections) = do 
            trans $ reverse section
            pushCode
            bindCode sections
            popCode

        boundGenerator = do 
            trans $ reverse constData
            newData <- gets $ head . code 
            modify $ \ ctx -> ctx {code = [[]], constData = newData}
            bindCode oldCode 
