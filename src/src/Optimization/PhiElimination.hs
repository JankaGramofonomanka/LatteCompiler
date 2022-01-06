{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Optimization.PhiElimination where

import Control.Monad.State
import Data.List

import LLVM.LLVM
import Optimization.Overwrite
import Optimization.RemoveAss
import LLVM.Print
import Dependent

eliminatePhisFunc :: Func t ts -> Func t ts
eliminatePhisFunc func = go func where

    go :: Func t ts -> Func t ts
    go func@(Func retT argTs params fLabel blocks) = do
      foldl goBlock func blocks

    goBlock :: Func t ts -> SimpleBlock -> Func t ts
    goBlock toClean (SimpleBlock _ instrs _) = foldl goComInstr toClean instrs

    goComInstr :: Func t ts -> ComSimpleInstr -> Func t ts
    goComInstr toClean (Comment _) = toClean
    goComInstr toClean (Instr instr _) = goInstr toClean instr

    goInstr :: Func t ts ->  SimpleInstr -> Func t ts
    goInstr toClean instr = do

      case instr of
        Ass reg expr -> case expr of
          Phi t valList -> case vals valList of

            -- if all values are the same, then overwrite the register
            [val] -> do
              let func1 = overwrite t reg val toClean
              let func2 = removeAss reg func1
              go func2
            
            _ -> toClean

          _ -> toClean

        _ -> toClean

      where
        vals :: [(Label, Value t)] -> [Value t]
        vals = nub . map snd


eliminatePhisProg :: LLVMProg -> LLVMProg
eliminatePhisProg LLVM { mainFunc, funcs, .. }
  = LLVM
    { mainFunc = eliminatePhisFunc mainFunc
    , funcs = map eliminatePhisSomeFunc funcs
    , ..
    }

  where
    eliminatePhisSomeFunc :: SomeFunc -> SomeFunc
    eliminatePhisSomeFunc (t :&&: func) = t :&&: eliminatePhisFunc func
