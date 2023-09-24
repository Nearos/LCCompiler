
module RegAlloc(regAllocNaive, regAlloc) where

import GenRep
import PrintCode (Printable(..))

import RegAlloc.Naive ( regAllocNaive )
import RegAlloc.LiveRegister (LiveAnnotated)
import RegAlloc.InstructionTransactions (RegisterTransaction (..), instructionTransaction, InstructionControl (Funcall), instructionJump)

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust, isJust)

import Control.Monad.Trans.State ( evalStateT, gets, modify, get )
import Control.Monad (forM_, forM, when)
import Control.Monad.Trans.Class ( MonadTrans(lift) )

-- x0 -- x32 
-- x0 abi to pass closure information
-- x1 function argument 
-- x0 return value
-- x29, x30 return address
-- sp stack pointer - not an x register
-- x2, x3, and x4 are for loading spilled virtual registers

-- [x5 .. x28] are available 
availableRegisters = map HardX [5 .. 28]

-- v1 -> [v2, v3, v4, v5, ... ]
type LiveRegisterGraph var = M.Map var (S.Set var)

data HardRegister
    = HardX Int
    | HardSP
    | HardXZ
    deriving (Eq, Ord)

instance Printable HardRegister where
    printCode (HardX n) = "x" ++ show n
    printCode HardSP = "sp"
    printCode HardXZ = "xzr"

type StackOffset = Int

regAlloc :: [LiveAnnotated VirtualRegister] -> Generator (ARM64 HardRegister) ()
regAlloc ((_, lbl@(DefLabel _)) : inInst) = do
    emitCode $ fmap undefined lbl
    -- save return address to stack 
    emitCode $ InstFlex "stp" [In $ HardX 29, In $ HardX 30] $ DerefPre HardSP (-16)
    -- reserve stack space for spilled registers
    emitCode $ InstFlex "sub" [Out HardSP, In HardSP] $ ImmInt stackSpace

    mapM_ (regAssignInstruction stackSpace spilledAssignment virtualRegisterAssignment) inInst

    
    where
        liveGraph = generateLiveGraph inInst

        (spilled, allocationGraph) = spillRegisters (length availableRegisters) liveGraph

        virtualRegisterAssignment = allocateHardRegisters allocationGraph availableRegisters

        numberSpilled = length spilled
        stackSpace = 8 * if even numberSpilled then numberSpilled + 2 else numberSpilled + 1

        spilledAssignment = M.fromList $ zip spilled $ map (*8) [1..] -- (v3, 8), (v4, 16), (v6, 24), ...

regAlloc [] = return ()
regAlloc (a:xs) = do 
    emitCode $ undefined <$> snd a
    regAlloc xs  

allocateHardRegisters :: LiveRegisterGraph VirtualRegister -> [HardRegister] -> M.Map VirtualRegister HardRegister
allocateHardRegisters graph availableRegisters = M.foldlWithKey go M.empty graph
    where
        go :: M.Map VirtualRegister HardRegister -> VirtualRegister -> S.Set VirtualRegister -> M.Map VirtualRegister HardRegister
        go allocationSoFar registerToBeAllocated edges =
            let
                usedHardRegisters = S.unions $ S.map (\target -> maybe S.empty S.singleton $ M.lookup target allocationSoFar) edges
                freeHardRegisters = filter (`S.notMember` usedHardRegisters) availableRegisters
            in M.insert registerToBeAllocated (head freeHardRegisters) allocationSoFar

spillRegisters :: Int -> LiveRegisterGraph VirtualRegister -> ([VirtualRegister], LiveRegisterGraph VirtualRegister)
spillRegisters numberOfAvailableRegisters liveGraph
    | largestNumberOfEdges < numberOfAvailableRegisters = ([], liveGraph)
    | otherwise =
        let (recSpilledRegisters, recAllocationGraph) = spillRegisters numberOfAvailableRegisters filteredGraph
        in (mostNeigborsEdge : recSpilledRegisters, recAllocationGraph)
    where
        (mostNeigborsEdge, largestNumberOfEdges) = M.foldlWithKey checkForMax (Virt (-1), -1) liveGraph

        checkForMax :: (VirtualRegister, Int) -> VirtualRegister -> S.Set VirtualRegister -> (VirtualRegister, Int)
        checkForMax (oldReg, oldMax) reg edges
            | S.size edges > oldMax = (reg, S.size edges)
            | otherwise = (oldReg, oldMax)

        filteredGraph = M.map (S.\\ S.singleton mostNeigborsEdge) $ M.delete mostNeigborsEdge liveGraph


generateLiveGraph :: [LiveAnnotated VirtualRegister] -> LiveRegisterGraph VirtualRegister
generateLiveGraph = foldl updateGraph M.empty . map (S.filter isVirtual . fst)
    where
        updateGraph :: LiveRegisterGraph VirtualRegister -> S.Set VirtualRegister -> LiveRegisterGraph VirtualRegister
        updateGraph oldGraph live =
            let
                graphs = S.map (\key -> M.singleton key $ live S.\\ S.singleton key ) live
                thisInstructionGraph = S.fold (<>) graphs
            in S.foldl (M.unionWith (<>)) oldGraph graphs

regAssignInstruction
    :: Int
    -> M.Map VirtualRegister StackOffset
    -> M.Map VirtualRegister HardRegister
    -> LiveAnnotated VirtualRegister
    -> Generator (ARM64 HardRegister) ()
regAssignInstruction stackSpace _ _ (_, InstHiddenReg "ret" _ _) = do 
    -- restore stack pointer to how it was before space was reserved for spilled registers
    emitCode $ InstFlex "add" [Out HardSP, In HardSP] $ ImmInt stackSpace
    -- restore return adress from stack
    emitCode $ InstFlex "ldp" [Out $ HardX 29, Out $ HardX 30] $ DerefPost HardSP 16
    emitCode $ InstHiddenReg "ret" [] []

regAssignInstruction _ spilledAssignment virtualRegisterAssignment (live, instruction) = do
    flip evalStateT [] $ do
        forM_ (S.elems inRegs) $ \reg -> do
            findHardRegister reg
            case M.lookup reg spilledAssignment of
                Nothing -> return ()
                Just offset -> do
                    hardReg <- gets $ fromJust . lookup reg
                    lift $ emitCode $ InstFlex "ldr" [Out hardReg] $ DerefExpr HardSP offset
        forM_ (S.elems outRegs) findHardRegister
        assignment <- get
        preFunCall
        lift $ emitCode $ fromJust . flip lookup assignment <$> instruction
        postFunCall
        forM_ (S.elems outRegs) $ \ reg ->
            case M.lookup reg spilledAssignment of
                Just offset -> lift $ emitCode $ InstFlex "str" [In $ fromJust $ lookup reg assignment] $ DerefExpr HardSP offset
                Nothing -> return ()
    where
        RegisterTransaction inRegs outRegs = instructionTransaction instruction

        findHardRegister reg 
            | isVirtual reg = 
                case M.lookup reg virtualRegisterAssignment of
                    Just hard -> modify ((reg, hard):)
                    Nothing -> case M.lookup reg spilledAssignment of
                        Nothing -> modify ((reg, HardXZ):)
                        Just offset -> do
                            cAss <- get
                            let remainingSpillRegisters = filter (flip notElem $ map snd cAss) $ map HardX [2 .. 4]
                            modify ((reg, head remainingSpillRegisters):)
            | otherwise = 
                let hardR = 
                        case reg of 
                            X n -> HardX n
                            W n -> undefined
                            XZR -> undefined  
                            WZR -> undefined
                            SP -> HardSP
                in modify ((reg, hardR):)

        liveAssignedRegisters = S.elems $ S.map fromJust $ S.filter isJust $ S.map (`M.lookup` virtualRegisterAssignment) live
        liveAssignedRegistersStackSpace = 8 * if even $ length liveAssignedRegisters then length liveAssignedRegisters + 2 else length liveAssignedRegisters + 1

        isFunCall = instructionJump instruction == Funcall

        preFunCall = when isFunCall $ do 
            lift $ emitCode $ InstFlex "sub" [Out HardSP, In HardSP] $ ImmInt liveAssignedRegistersStackSpace
            forM_ (zip [1..] liveAssignedRegisters) $ \ (i, hr) -> do 
                lift $ emitCode $ InstFlex "str" [In hr] $ DerefExpr HardSP (i * 8)

        postFunCall = when isFunCall $ do 
            forM_ (zip [1..] liveAssignedRegisters) $ \ (i, hr) -> do 
                lift $ emitCode $ InstFlex "ldr" [Out hr] $ DerefExpr HardSP (i * 8)
            lift $ emitCode $ InstFlex "add" [Out HardSP, In HardSP] $ ImmInt liveAssignedRegistersStackSpace


-- Quick tests

instance Show VirtualRegister where
    show = printCode

-- >>> spillRegisters 3 $ generateLiveGraph $ map (\val -> (S.fromList val, Comment "ass")) [[Virt 1, Virt 2], [Virt 1, Virt 3], [Virt 1, Virt 4]]
-- ([v1],fromList [(v2,fromList []),(v3,fromList []),(v4,fromList [])])
