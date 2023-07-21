module CodeGen where 

import GenRep
import Tree
import Binding -- previous pass

import Control.Monad 
import Data.Maybe (fromMaybe, fromJust)

type Environment = [(Binding, VirtualRegister)]

genExpr :: Environment -> Expr Resolved -> Generator VirtualRegister VirtualRegister

genExpr ctx (Extern _ name) = do 
    emitCode $ Comment $ "loading external symbol " ++ name 
    externRegister <- getVReg
    emitCode $ Memory "ldr" externRegister $ LabelAddr name 
    return externRegister

genExpr _ (Var _ (Free name)) = genString name

genExpr _ (SrcStringLit _ value) = genString value

genExpr _ (SrcIntLit _ value) = genInt value

genExpr ctx (Var _ (Bound name id)) = 
    maybe undefined return $ lookup (name, id) ctx

genExpr ctx (App _ a b) = do 
    aReg <- genExpr ctx a 
    bReg <- genExpr ctx b 
    emitCode $ Comment "Load closure arg"
    emitCode $ TwoRegister "mov" abiArg bReg
    callClosure aReg

genExpr callerContext (Lambda meta (Bound name id) body) = do 
    label <- genBody contextStruct
    emitCode $ Comment "Initializing closure"
    initClosure contextStruct label
    where 

        addToContextStruct :: Struct -> Binding -> Struct 
        addToContextStruct (Struct existing) newCapture  = Struct $ (newCapture, 8) : existing 

        contextStruct = foldl addToContextStruct (Struct []) $ captures meta


        functionStart = do 
            emitCode $ Memory "str" (X 29) $ Preorder sp (-8)
            emitCode $ Memory "str" (X 30) $ Preorder sp (-8)

        functionEnd = do 
            emitCode $ Memory "ldr" (X 30) $ Postorder sp 8
            emitCode $ Memory "ldr" (X 29) $ Postorder sp 8
            emitCode $ PseudoZero "ret"
            
        loadEnvironment :: Struct -> Generator VirtualRegister Environment 
        loadEnvironment ctx = do 
            argReg <- getVReg
            emitCode $ Comment "load argument into register"
            emitCode $ TwoRegister "mov" argReg abiArg
            emitCode $ Comment "load closure environment into registers"
            context <- forM (captures meta) $ \ fieldBdg  -> do 
                fieldReg <- getVReg
                emitCode $ Memory "ldr" fieldReg $ ExprAddr abiCtx $ offsetOf ctx fieldBdg
                return (fieldBdg, fieldReg)
            return $ context ++ [((name, id), argReg)]

        genBody :: Struct -> Generator VirtualRegister String
        genBody contextStruct = do 
            label <- getLabel "lambda_body"
            pushCode -- this code is the function definition
            emitCode $ DefLabel label
            functionStart
            env <- loadEnvironment contextStruct
            emitCode $ Comment "function body expr"
            resultReg <- genExpr env body
            emitCode $ Comment "move result into return register"
            emitCode $ TwoRegister "mov" abiRet resultReg
            functionEnd
            popCode
            return label

        initContext :: Struct -> Generator VirtualRegister VirtualRegister
        initContext contextStruct = do 
            contextPtrReg <- getVReg
            emitCode $ Comment "allocating closure context struct"
            emitCode $ TwoImmediate "mov" abiArg $ IntLit $ sizeOf contextStruct
            emitCode $ OneImmediate "bl" $ Label "malloc"
            emitCode $ TwoRegister "mov" contextPtrReg abiRet
            emitCode $ Comment "populating closure context struct from current context"
            forM_ (captures meta) $ \ binding -> do 
                let addrInCtxStruct = ExprAddr contextPtrReg $ offsetOf contextStruct binding
                let currentRegister = fromMaybe undefined $ lookup binding callerContext
                emitCode $ Memory "str" currentRegister addrInCtxStruct
            return contextPtrReg
        
        initClosure :: Struct -> String -> Generator VirtualRegister VirtualRegister
        initClosure contextStruct label = do 
            closurePtrReg <- getVReg
            emitCode $ Comment "Allocating closure struct"
            emitCode $ TwoImmediate "mov" abiArg $ IntLit $ sizeOf closure
            emitCode $ OneImmediate "bl" $ Label "malloc"
            emitCode $ TwoRegister "mov" closurePtrReg abiRet

            emitCode $ Comment "loading function body address"
            codePtrReg <- getVReg
            emitCode $ Memory "ldr" codePtrReg $ LabelAddr label

            contextPtrReg <- initContext contextStruct 

            emitCode $ Comment "populating closure struct"
            emitCode $ Memory "str" codePtrReg $ ExprAddr closurePtrReg $ offsetOf closure ("fn", -1)
            emitCode $ Memory "str" contextPtrReg $ ExprAddr closurePtrReg $ offsetOf closure ("context", -1)

            return closurePtrReg

genString :: String -> Generator VirtualRegister VirtualRegister
genString name = do 
    emitCode $ Comment $ "load string \"" ++ name ++ "\""
    contentLabel <- getLabel "string_content"
    emitData $ Pseudo ".align" $ IntLit 3
    emitData $ DefLabel contentLabel 
    emitData $ Pseudo ".ascii" $ StringLit name 

    structLabel <- getLabel "string_struct"
    emitData $ Pseudo ".align" $ IntLit 3
    emitData $ DefLabel structLabel 
    emitData $ Pseudo ".quad" $ IntLit $ length name 
    emitData $ Pseudo ".quad" $ Label contentLabel

    ret <- getVReg
    emitCode $ Memory "ldr" ret $ LabelAddr structLabel 
    return ret

genInt :: Int -> Generator VirtualRegister VirtualRegister
genInt val = do 
    label <- getLabel "int_lit"
    emitData $ Pseudo ".align" $ IntLit 3
    emitData $ DefLabel label
    emitData $ Pseudo ".quad" $ IntLit val 
    retReg <- getVReg
    emitCode $ Memory "ldr" retReg $ LabelAddr label
    return retReg


{-# DEPRECATED #-}
genAST :: Expr Resolved -> Generator VirtualRegister ()
genAST expr = do 
    emitCode $ Pseudo ".global" $ Label "_start"
    emitCode $ DefLabel "_start"
    result <- genExpr [] expr
    emitCode $ TwoRegister "mov" abiArg result 
    emitCode $ OneImmediate "bl" $ Label "print_message"
    emitCode $ OneImmediate "bl" $ Label "print_newline"
    emitCode $ OneImmediate "bl" $ Label "exit"

genProgram :: [Toplevel Resolved] -> Generator VirtualRegister ()
genProgram program = do 
        emitData $ Pseudo ".align" $ IntLit 3 -- align data segment
        emitCode $ Pseudo ".global" $ Label "_start"
        emitCode $ DefLabel "_start"
        envt <- genProgramEnvironment [] program
        let mainReg = fromJust $ lookup ("main", -1) envt 
        emitCode $ TwoRegister "mov" abiArg mainReg 
        emitCode $ OneImmediate "bl" $ Label "print_message"
        emitCode $ OneImmediate "bl" $ Label "print_newline"
        emitCode $ OneImmediate "bl" $ Label "exit"
    where 
        genProgramEnvironment envt [] = return envt
        genProgramEnvironment envt (Binding _ (Bound name id) expr : rest) = do 
            emitCode $ Comment $ "Generating global symbol " ++ name
            bindingReg <- getVReg
            let envt' = ((name, id), bindingReg) : envt 
            exprReg <- genExpr envt' expr 
            emitCode $ TwoRegister "mov" bindingReg exprReg
            genProgramEnvironment envt' rest