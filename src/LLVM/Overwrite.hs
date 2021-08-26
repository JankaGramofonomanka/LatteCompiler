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

module LLVM.Overwrite where

import Data.Singletons.Prelude
import Data.GADT.Compare

import LLVM.LLVM
import LLVM.TypeConversion

import Dependent

class HasRegisters a where
  overwrite :: Sing t -> Reg t -> Value t -> a -> a

instance HasRegisters ComBranchInstr where
  overwrite singT reg val (BrInstr instr cmt)
      = BrInstr (overwrite singT reg val instr) cmt

instance HasRegisters BranchInstr where
  overwrite singT reg val instr = case instr of
    CondBranch cond labelIf labelElse -> case gcompare singT (sing @(I 1)) of
      
      GEQ -> CondBranch cond' labelIf labelElse where
              cond' = if cond == Var reg then val else cond

      _ -> instr
  
    Ret t v -> case gcompare singT t of

      GEQ -> if v == Var reg then Ret t val else Ret t v
      _ -> instr

    _ -> instr

instance HasRegisters ComSimpleInstr where
  overwrite singT reg val (Comment cmt) = Comment cmt
  overwrite singT reg val (Instr instr cmt)
    = Instr (overwrite singT reg val instr) cmt

instance HasRegisters SimpleInstr where
  overwrite singT reg val instr = case instr of
    Ass r expr -> Ass r (overwrite singT reg val expr)
    VoidExpr expr -> VoidExpr (overwrite singT reg val expr)

instance HasRegisters (Expr a) where

  overwrite singT reg val expr = case expr of

    BinOperation t op lhs rhs -> BinOperation t op lhs' rhs' where
      lhs' = case gcompare t singT of
        GEQ -> if lhs == Var reg then val else lhs
        _ -> lhs
      
      rhs' = case gcompare t singT of
        GEQ -> if rhs == Var reg then val else rhs
        _ -> rhs
      
    Call t f argTs args -> Call t f argTs args' where
      args' = overwriteDList argTs args

      overwriteDList
        :: SList ts
        -> ArgList ts
        -> ArgList ts
      overwriteDList SNil DNil = DNil
      overwriteDList (SCons t ts) (arg :> args)
        = arg' :> overwriteDList ts args where
          arg' = case gcompare t singT of
            GEQ -> if arg == Var reg then val else arg
            _ -> arg


    GetElemPtr t tIndex container index -> 
      GetElemPtr t tIndex container' index' where
        container' = case gcompare (SPtr t) singT of
          GEQ -> if container == Var reg then val else container
          _ -> container
        
        index' = case gcompare tIndex singT of
          GEQ -> if index == Var reg then val else index
          _ -> index

    GetArrElemPtr tArr tIndex1 tIndex2 arr index1 index2
      -> GetArrElemPtr tArr tIndex1 tIndex2 arr' index1' index2' where

        arr' = case gcompare (SPtr tArr) singT of
          GEQ -> if arr == Var reg then val else arr
          _ -> arr
        
        index1' = case gcompare tIndex1 singT of
          GEQ -> if index1 == Var reg then val else index1
          _ -> index1

        index2' = case gcompare tIndex2 singT of
          GEQ -> if index2 == Var reg then val else index2
          _ -> index2

    ICMP t kind lhs rhs -> ICMP t kind lhs' rhs' where

      lhs' = case gcompare t singT of
        GEQ -> if lhs == Var reg then val else lhs
        _ -> lhs
      
      rhs' = case gcompare t singT of
        GEQ -> if rhs == Var reg then val else rhs
        _ -> rhs

    Phi t vals -> Phi t vals' where
      vals' = overwriteList vals
      
      overwriteList [] = []
      overwriteList ((label, v) : vs) = (label, v') : overwriteList vs where

        v' = case gcompare t singT of
          GEQ -> if v == Var reg then val else v
          _ -> v


instance HasRegisters SimpleBlock where
  overwrite singT reg val SimpleBlock { body = body, lastInstr = brInstr, .. }
    = SimpleBlock { body = body', lastInstr = brInstr', .. }

    where
      body' = map (overwrite singT reg val) body
      brInstr' = overwrite singT reg val brInstr




