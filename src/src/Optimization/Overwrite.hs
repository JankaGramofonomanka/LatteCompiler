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

module Optimization.Overwrite where

import Data.Singletons.Prelude
import Data.GADT.Compare

import LLVM.LLVM
import LLVM.TypeConversion

import Dependent

class HasRegisters a where
  overwrite :: Sing t -> Reg t -> Value t -> a -> a

overwriteVal :: Reg t -> Value t -> Value t -> Value t
overwriteVal reg toWrite original
  = if original == Var reg then toWrite else original

instance HasRegisters ComBranchInstr where
  overwrite singT reg val (BrInstr instr cmt)
      = BrInstr (overwrite singT reg val instr) cmt

instance HasRegisters BranchInstr where
  overwrite singT reg val instr = case instr of
    CondBranch cond labelIf labelElse -> case gcompare singT (sing @(I 1)) of
      
      GEQ -> CondBranch cond' labelIf labelElse where
              cond' = overwriteVal reg val cond

      _ -> instr
  
    Ret t v -> case gcompare singT t of

      GEQ -> Ret t $ overwriteVal reg val v
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
    
    Store vT v dest -> case gcompare singT vT of
      GEQ -> Store vT (overwriteVal reg val v) dest
      
      _   -> case gcompare singT (SPtr vT) of
        GEQ -> Store vT v (overwriteVal reg val dest)
        _   -> instr

instance HasRegisters (Expr a) where

  overwrite singT reg val expr = case expr of

    BinOperation t op lhs rhs -> BinOperation t op lhs' rhs' where
      lhs' = case gcompare t singT of
        GEQ -> overwriteVal reg val lhs
        _ -> lhs
      
      rhs' = case gcompare t singT of
        GEQ -> overwriteVal reg val rhs
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
            GEQ -> overwriteVal reg val arg
            _ -> arg


    GetElemPtr t tIndex container index -> 
      GetElemPtr t tIndex container' index' where
        container' = case gcompare (SPtr t) singT of
          GEQ -> overwriteVal reg val container
          _ -> container
        
        index' = case gcompare tIndex singT of
          GEQ -> overwriteVal reg val index
          _ -> index

    GetArrElemPtr tArr tIndex1 tIndex2 arr index1 index2
      -> GetArrElemPtr tArr tIndex1 tIndex2 arr' index1' index2' where

        arr' = case gcompare (SPtr tArr) singT of
          GEQ -> overwriteVal reg val arr
          _ -> arr
        
        index1' = case gcompare tIndex1 singT of
          GEQ -> overwriteVal reg val index1
          _ -> index1

        index2' = case gcompare tIndex2 singT of
          GEQ -> overwriteVal reg val index2
          _ -> index2


    GetAttrPtr structT indexT1 indexT2 structPtr index1 index2
      -> GetAttrPtr structT indexT1 indexT2 structPtr' index1' index2' where

        structPtr' = case gcompare singT (SPtr structT) of
          GEQ -> overwriteVal reg val structPtr
          _   -> structPtr

        index1' = case gcompare singT indexT1 of
          GEQ -> overwriteVal reg val index1
          _   -> index1

        index2' = case gcompare singT indexT2 of
          GEQ -> overwriteVal reg val index2
          _   -> index2



    GetArrAttrPtr arrStructT indexT1 indexT2 arrStructPtr index1 index2
      -> GetArrAttrPtr arrStructT indexT1 indexT2 arrStructPtr' index1' index2'
        where
          arrStructPtr' = case gcompare singT (SPtr arrStructT) of
            GEQ -> overwriteVal reg val arrStructPtr
            _   -> arrStructPtr

          index1' = case gcompare singT indexT1 of
            GEQ -> overwriteVal reg val index1
            _   -> index1

          index2' = case gcompare singT indexT2 of
            GEQ -> overwriteVal reg val index2
            _   -> index2

    ICMP t kind lhs rhs -> ICMP t kind lhs' rhs' where

      lhs' = case gcompare t singT of
        GEQ -> overwriteVal reg val lhs
        _ -> lhs
      
      rhs' = case gcompare t singT of
        GEQ -> overwriteVal reg val rhs
        _ -> rhs

    Phi t vals -> Phi t vals' where
      vals' = overwriteList vals
      
      overwriteList [] = []
      overwriteList ((label, v) : vs) = (label, v') : overwriteList vs where

        v' = case gcompare t singT of
          GEQ -> overwriteVal reg val v
          _   -> v

    Load valT src -> Load valT src' where
      src' = case gcompare (SPtr valT) singT of
        GEQ -> overwriteVal reg val src
        _   -> src

    BitCast inputT input outputT -> BitCast inputT input' outputT where
      input' = case gcompare singT inputT of
        GEQ -> overwriteVal reg val input
        _   -> input


instance HasRegisters SimpleBlock where
  overwrite singT reg val SimpleBlock { body = body, lastInstr = brInstr, .. }
    = SimpleBlock { body = body', lastInstr = brInstr', .. }

    where
      body' = map (overwrite singT reg val) body
      brInstr' = overwrite singT reg val brInstr


instance HasRegisters (Func t ts) where
  overwrite singT reg val (Func retT argTs params fLabel body)
    = Func retT argTs params fLabel body' where
      body' = map (overwrite singT reg val) body

