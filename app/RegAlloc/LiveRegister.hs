module RegAlloc.LiveRegister (findLiveRegs, LiveAnnotated) where

import GenRep
import RegAlloc.InstructionTransactions
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

type LiveAnnotated reg = (S.Set reg, ARM64 reg)

findLiveRegs :: Ord reg => [ARM64 reg] -> [LiveAnnotated reg]
findLiveRegs = go S.empty M.empty
    where 
        go :: Ord reg => S.Set reg -> M.Map String (S.Set reg) ->  [ARM64 reg] -> [LiveAnnotated reg]
        go liveAfter labels (inst@(DefLabel label) : rest) = (liveAfter, inst) : go liveAfter (M.insert label liveAfter labels) rest 
        go liveAfter labels (inst : rest) =
            let 
                liveAfterCFG = case instructionJump inst of 
                    Jump label -> S.union liveAfter $ fromJust $ M.lookup label labels 
                    CondJump label -> S.union liveAfter $ fromJust $ M.lookup label labels 
                    _ -> liveAfter
                RegisterTransaction consumes produces = instructionTransaction inst 
                liveBefore = (liveAfterCFG S.\\ produces) `S.union` consumes
            in
                (liveBefore, inst) : go liveBefore labels rest

        go _ _ [] = []

