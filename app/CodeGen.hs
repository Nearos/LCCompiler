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

        initContext :: VirtualRegister -> Struct -> Generator VirtualRegister VirtualRegister
        initContext closurePtrReg contextStruct = do 
            contextPtrReg <- getVReg
            emitCode $ Comment "allocating closure context struct"
            emitCode $ TwoImmediate "mov" abiArg $ IntLit $ sizeOf contextStruct
            emitCode $ OneImmediate "bl" $ Label "malloc"
            emitCode $ TwoRegister "mov" contextPtrReg abiRet
            emitCode $ Comment "populating closure context struct from current context"
            let selfBinding = case rec_bind meta of 
                    Just bnd ->[(bnd, closurePtrReg)]
                    Nothing -> []
            let closureContext = selfBinding ++ callerContext
            forM_ (captures meta) $ \ binding -> do 
                let addrInCtxStruct = ExprAddr contextPtrReg $ offsetOf contextStruct binding
                let currentRegister = fromMaybe undefined $ lookup binding closureContext
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

            contextPtrReg <- initContext closurePtrReg contextStruct 

            emitCode $ Comment "populating closure struct"
            emitCode $ Memory "str" codePtrReg $ ExprAddr closurePtrReg $ offsetOf closure ("fn", -1)
            emitCode $ Memory "str" contextPtrReg $ ExprAddr closurePtrReg $ offsetOf closure ("context", -1)

            return closurePtrReg

genExpr ctx (Construct _ (Bound nm id) subexprs) = do 
    closurePtrReg <- getVReg 
    emitCode $ Comment $ "Allocating alternative struct for " ++ nm
    emitCode $ TwoImmediate "mov" abiArg $ IntLit $ 8 + 8 * length subexprs
    emitCode $ OneImmediate "bl" $ Label "malloc"
    emitCode $ TwoRegister "mov" closurePtrReg abiRet
    constValueReg <- getVReg
    emitCode $ Comment "put alternative label value in struct"
    emitCode $ TwoImmediate "mov" constValueReg $ IntLit id 
    emitCode $ Memory "str" constValueReg $ RegAddr closurePtrReg
    emitCode $ Comment "putting argument values in struct"
    forM_ (zip [1..] subexprs) $ \ (num, sexpr) -> do 
        subExprReg <- genExpr ctx sexpr
        emitCode $ Memory "str" subExprReg $ ExprAddr closurePtrReg $ 8 * num
    return closurePtrReg

genExpr ctx (Case meta scrut branches) = do 
    emitCode $ Comment "case expression scrutinee"
    scrutEvaluated <- genExpr ctx scrut 
    altValue <- getVReg
    emitCode $ Comment "load discriminating value from memory"
    emitCode $ Memory "ldr" altValue $ RegAddr scrutEvaluated
    retValue <- getVReg
    endLabel <- getLabel "after_case_alternatives"
    labels <- forM branches $ \ (CaseBranch (Pattern (Bound name id) _) _) -> do 
        label <- getLabel "case_alternative"
        emitCode $ Comment $ "checking for alternative for " ++ name
        emitCode $ TwoImmediate "cmp" altValue $ IntLit id
        emitCode $ OneImmediate "beq" $ Label label
        return label
    emitCode $ Comment "alternatives code"
    emitCode $ OneImmediate "b" $ Label endLabel
    forM_ (zip labels branches) $ \ (branchLabel, CaseBranch (Pattern _ bindings) expr) -> do
        emitCode $ DefLabel branchLabel 
        emitCode $ Comment "loading pattern bindings"
        -- genrate espression with bindings in context
        newContext <- forM (zip [1..] bindings) $ \ (offsetIndex, Bound name id) -> do 
            bindingReg <- getVReg
            emitCode $ Memory "ldr" bindingReg $ ExprAddr scrutEvaluated $ offsetIndex * 8
            return ((name, id), bindingReg)
        let innerContext = newContext ++ ctx
        emitCode $ Comment "case inner expression"
        exprRet <- genExpr innerContext expr
        emitCode $ TwoRegister "mov" retValue exprRet
        emitCode $ OneImmediate "b" $ Label endLabel
    emitCode $ DefLabel endLabel
    return retValue


genString :: String -> Generator VirtualRegister VirtualRegister
genString name = do 
    emitCode $ Comment $ "load string \"" ++ name ++ "\""
    contentLabel <- getLabel "string_content"
    emitData $ DefLabel contentLabel 
    emitData $ Pseudo ".ascii" $ StringLit name 

    structLabel <- getLabel "string_struct"
    emitData $ DefLabel structLabel 
    emitData $ Pseudo ".quad" $ IntLit $ length name 
    emitData $ Pseudo ".quad" $ Label contentLabel

    ret <- getVReg
    emitCode $ Memory "ldr" ret $ LabelAddr structLabel 
    return ret

genInt :: Int -> Generator VirtualRegister VirtualRegister
genInt val = do 
    label <- getLabel "int_lit"
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
            exprReg <- genExpr envt expr 
            let envt' = ((name, id), bindingReg) : envt
            emitCode $ TwoRegister "mov" bindingReg exprReg
            genProgramEnvironment envt' rest
        genProgramEnvironment envt (ConstDef {} : rest) = genProgramEnvironment envt rest 