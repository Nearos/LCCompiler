module CodeGen where 

import GenRep
import Tree
import Binding -- previous pass

import Control.Monad 
import Data.Maybe (fromMaybe, fromJust)

type Environment = [(Binding, VirtualRegister)]

allocate :: Int -> Generator VirtualRegister VirtualRegister
allocate n = do
    retReg <- getVReg
    emitCode $ InstFlex "mov" [Out abiArg] $ ImmInt n 
    emitCode $ funcallFlex "bl" [] $ ImmLabel "malloc"
    emitCode $ InstRegs "mov" [Out retReg, In abiRet]
    return retReg

enstructure :: VirtualRegister -> [VirtualRegister] -> Generator VirtualRegister ()
enstructure memoryLocation registers = do 
    forM_ (zip [0..] registers) $ \ (offsetNumber, register) -> do 
        emitCode $ InstFlex "str" [In register] $ DerefExpr memoryLocation $ offsetNumber * 8

genExpr :: Environment -> Expr Resolved -> Generator VirtualRegister VirtualRegister

genExpr ctx (Extern _ name) = do 
    emitCode $ Comment $ "loading external symbol " ++ name 
    externRegister <- getVReg
    emitCode $ InstFlex "ldr" [Out externRegister] $ LabelAddr name 
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
    emitCode $ InstRegs "mov" [Out abiArg, In bReg]
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
            emitCode $ InstFlex "str" [In (X 29)] $ DerefPre sp (-8)
            emitCode $ InstFlex "str" [In (X 30)] $ DerefPre sp (-8)

        functionEnd = do 
            emitCode $ InstFlex "ldr" [Out (X 30)] $ DerefPost sp 8
            emitCode $ InstFlex "ldr" [Out (X 29)] $ DerefPost sp 8
            emitCode $ InstHiddenReg "ret" [In (X 29), In (X 30), In abiRet] []
            
        loadEnvironment :: Struct -> Generator VirtualRegister Environment 
        loadEnvironment ctx = do 
            argReg <- getVReg
            emitCode $ Comment "load argument into register"
            emitCode $ InstRegs "mov" [Out argReg, In abiArg]
            emitCode $ Comment "load closure environment into registers"
            context <- forM (captures meta) $ \ fieldBdg  -> do 
                fieldReg <- getVReg
                emitCode $ InstFlex "ldr" [Out fieldReg] $ DerefExpr abiCtx $ offsetOf ctx fieldBdg
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
            emitCode $ InstRegs "mov" [Out abiRet, In resultReg]
            functionEnd
            popCode
            return label

        initContext :: VirtualRegister -> Struct -> Generator VirtualRegister VirtualRegister
        initContext closurePtrReg contextStruct = do 
            emitCode $ Comment "allocating closure context struct"
            contextPtrReg <- allocate $ sizeOf contextStruct
            emitCode $ Comment "populating closure context struct from current context"
            let selfBinding = case rec_bind meta of 
                    Just bnd ->[(bnd, closurePtrReg)]
                    Nothing -> []
            let closureContext = selfBinding ++ callerContext
            forM_ (captures meta) $ \ binding -> do 
                let addrInCtxStruct = DerefExpr contextPtrReg $ offsetOf contextStruct binding
                let currentRegister = fromMaybe undefined $ lookup binding closureContext
                emitCode $ InstFlex "str" [In currentRegister] addrInCtxStruct
            return contextPtrReg
        
        initClosure :: Struct -> String -> Generator VirtualRegister VirtualRegister
        initClosure contextStruct label = do 
            closurePtrReg <- getVReg
            emitCode $ Comment "Allocating closure struct"
            closurePtrReg <- allocate $ sizeOf closure

            emitCode $ Comment "loading function body address"
            codePtrReg <- getVReg
            emitCode $ InstFlex "ldr" [Out codePtrReg] $ LabelAddr label

            contextPtrReg <- initContext closurePtrReg contextStruct 

            emitCode $ Comment "populating closure struct"
            enstructure closurePtrReg [codePtrReg, contextPtrReg]
            
            return closurePtrReg

genExpr ctx (Construct _ (Bound nm id) subexprs) = do 
    emitCode $ Comment $ "Allocating alternative struct for " ++ nm
    structPtrReg <- allocate $ 8 + 8 * length subexprs

    constValueReg <- getVReg
    emitCode $ Comment "put alternative label value in struct"
    emitCode $ InstFlex "mov" [Out constValueReg] $ ImmInt id 

    subExprRegs <- mapM (genExpr ctx) subexprs

    enstructure structPtrReg (constValueReg : subExprRegs)
    return structPtrReg

genExpr ctx (Case meta scrut branches) = do 
    emitCode $ Comment "case expression scrutinee"
    scrutEvaluated <- genExpr ctx scrut 
    altValue <- getVReg
    emitCode $ Comment "load discriminating value from memory"
    emitCode $ InstFlex "ldr" [Out altValue] $ DerefReg scrutEvaluated
    retValue <- getVReg
    endLabel <- getLabel "after_case_alternatives"
    labels <- forM branches $ \ (CaseBranch (Pattern (Bound name id) _) _) -> do 
        label <- getLabel "case_alternative"
        emitCode $ Comment $ "checking for alternative for " ++ name
        emitCode $ InstFlex "cmp" [In altValue] $ ImmInt id
        emitCode $ InstFlex "beq" [] $ ImmLabel label
        return label
    emitCode $ Comment "alternatives code"
    emitCode $ InstFlex "b" []  $ ImmLabel endLabel
    forM_ (zip labels branches) $ \ (branchLabel, CaseBranch (Pattern _ bindings) expr) -> do
        emitCode $ DefLabel branchLabel 
        emitCode $ Comment "loading pattern bindings"
        -- genrate espression with bindings in context
        newContext <- forM (zip [1..] bindings) $ \ (offsetIndex, Bound name id) -> do 
            bindingReg <- getVReg
            emitCode $ InstFlex "ldr" [Out bindingReg] $ DerefExpr scrutEvaluated $ offsetIndex * 8
            return ((name, id), bindingReg)
        let innerContext = newContext ++ ctx
        emitCode $ Comment "case inner expression"
        exprRet <- genExpr innerContext expr
        emitCode $ InstRegs "mov" [Out retValue, In exprRet]
        emitCode $ InstFlex "b" [] $ ImmLabel endLabel
    emitCode $ DefLabel endLabel
    return retValue


genString :: String -> Generator VirtualRegister VirtualRegister
genString name = do 
    emitCode $ Comment $ "load string \"" ++ name ++ "\""
    contentLabel <- getLabel "string_content"
    emitData $ DefLabel contentLabel 
    emitData $ Pseudo ".ascii" $ ImmString name 

    structLabel <- getLabel "string_struct"
    emitData $ DefLabel structLabel 
    emitData $ Pseudo ".quad" $ ImmInt $ length name 
    emitData $ Pseudo ".quad" $ ImmLabel contentLabel

    ret <- getVReg
    emitCode $ InstFlex "ldr" [Out ret] $ LabelAddr structLabel 
    return ret

genInt :: Int -> Generator VirtualRegister VirtualRegister
genInt val = do 
    label <- getLabel "int_lit"
    emitData $ DefLabel label
    emitData $ Pseudo ".quad" $ ImmInt val 
    retReg <- getVReg
    emitCode $ InstFlex "ldr" [Out retReg] $ LabelAddr label
    return retReg

genProgram :: [Toplevel Resolved] -> Generator VirtualRegister ()
genProgram program = do 
        emitCode $ Pseudo ".global" $ ImmLabel "_start"
        emitCode $ DefLabel "_start"
        envt <- genProgramEnvironment [] program
        let mainReg = fromJust $ lookup ("main", -1) envt 
        emitCode $ InstRegs "mov" [Out abiArg, In mainReg]
        emitCode $ funcallFlex "bl" [] $ ImmLabel "print_message"
        emitCode $ funcallFlex "bl" [] $ ImmLabel "print_newline"
        emitCode $ funcallFlex "bl" [] $ ImmLabel "exit"
    where 
        genProgramEnvironment envt [] = return envt
        genProgramEnvironment envt (Binding _ (Bound name id) expr : rest) = do 
            emitCode $ Comment $ "Generating global symbol " ++ name
            bindingReg <- getVReg
            exprReg <- genExpr envt expr 
            let envt' = ((name, id), bindingReg) : envt
            emitCode $ InstRegs "mov" [Out bindingReg, In exprReg]
            genProgramEnvironment envt' rest
        genProgramEnvironment envt (ConstDef {} : rest) = genProgramEnvironment envt rest 