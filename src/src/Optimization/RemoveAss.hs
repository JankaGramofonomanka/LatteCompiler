{-# LANGUAGE
    RecordWildCards
  , FlexibleContexts
  
  , DataKinds
  , ScopedTypeVariables
  , KindSignatures
  , TypeApplications
  , TypeOperators
  , TypeFamilies
  , GADTs
  , StandaloneKindSignatures
#-}

module Optimization.RemoveAss where

import Data.Singletons.Prelude
import Data.GADT.Compare

import LLVM.LLVM
import LLVM.TypeConversion

import Dependent
import LLVM.LLVM (SimpleBlock(SimpleBlock))


class HasAss a where
  removeAss :: Reg t -> a -> a

instance HasAss SimpleBlock where
  removeAss reg SimpleBlock { body = body, .. } 
    = SimpleBlock { body = body', .. } where
      body' = foldl remInit [] body

      remInit acc (Comment c) = acc ++ [Comment c]
      remInit acc instr@(Instr ins c) = case ins of
        Ass r e -> if regEq r reg then acc else acc ++ [instr]
        _ -> acc ++ [instr]
      
      -- equaliy of registers with possibly different types to be used under
      -- the assumption that two registers of different types won't have the 
      -- same names
      regEq :: Reg t1 -> Reg t2 -> Bool
      regEq reg1 reg2 = case (reg1, reg2) of
        (Reg r1 n1,     Reg r2 n2)      -> r1 == r2 && n1 == n2 
        (SpecialReg r1, SpecialReg r2)  -> r1 == r2
        _                               -> False

instance HasAss (Func t ts) where
  removeAss reg (Func retT argTs params fLabel body)
    = Func retT argTs params fLabel body' where

      body' = map (removeAss reg) body