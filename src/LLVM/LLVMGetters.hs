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
module LLVM.LLVMGetters where


import Prelude hiding ( EQ )
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error, SLT, SGT )
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM

import LLVM.LLVM
import qualified Syntax.SyntaxDep as DS
import LLVM.State
import LLVM.StateUtils
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent
import BuiltIns




-------------------------------------------------------------------------------
true, false :: Value ('I 1)
true = BoolLit True
false = BoolLit False




getVarValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.SLatteType t -> DS.Var t -> m (Value (GetPrimType t))
getVarValue singT var = case var of
  DS.Var p x -> do
    l <- getCurrentBlockLabel
    getIdentValue (typedIdent singT x) l

  DS.Attr {} -> throwTODOP (position var)
  DS.Elem {} -> throwTODOP (position var)
  DS.Null {} -> throwTODOP (position var)
  DS.Self {} -> throwTODOP (position var)



getExprValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.Expr t -> m (Value (GetPrimType t))
getExprValue expr = case expr of
  DS.EVar p singT v -> getVarValue singT v

  ---------------------------------------------------------------------
  DS.ELitInt  p i -> return (ILit i)
  DS.ELitBool p b -> return (BoolLit b)
  DS.EString  p s -> do
    (n :&: ptr) <- getStrLitConstPtr s
    reg <- getNewRegDefault

    let singT = SArray (sing @(I 8)) n
    addInstr $ Ass reg $ GetElemPtr singT i32 ptr (ILit 0)
    return $ Var reg

  ---------------------------------------------------------------------
  DS.EApp p t f argTs args -> case f of
    DS.Func _ funcId -> do
      let funcLabel = FuncLabel (name funcId)
      (argTypes, argList) <- getArgs argTs args
      reg <- getNewRegDefault

      let singT = sGetPrimType t
      addInstr $ Ass reg $ Call singT funcLabel argTypes argList
      return $ Var reg


    DS.Method {} -> throwTODOP p

  
  ---------------------------------------------------------------------
  DS.Neg p e -> do
    v <- getExprValue e
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 MUL (ILit (-1)) v
    return $ Var reg
    
  DS.Not p e -> do
    v <- getExprValue e
    reg <- getNewRegDefault

    cond <- getNewRegDefault
    (labelIf, labelElse, labelJoin) <- getIfElseLabels
    
    addInstr $ Ass cond $ ICMP i1 EQ v true
    finishBlock (CondBranch (Var cond) labelIf labelElse)
    
    newBlock labelIf
    finishBlock $ Branch labelJoin
    newBlock labelElse
    finishBlock $ Branch labelJoin
    newBlock labelJoin

    addInstr $ Ass reg
      $ Phi i1 [(labelIf, false), (labelElse, true)]
    return $ Var reg

  ---------------------------------------------------------------------
  DS.EOp p op e1 e2 -> do
    v1 <- getExprValue e1
    v2 <- getExprValue e2
    let binOp = getBinOp op
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 binOp v1 v2
    return $ Var reg

  DS.ERel p t op e1 e2 -> case t of
      DS.STInt -> do
        v1 <- getExprValue e1
        v2 <- getExprValue e2
        let cmpKind = getCMPKind op
        reg <- getNewRegDefault
    
        addInstr $ Ass reg $ ICMP i32 cmpKind v1 v2
        return $ Var reg
      
      -- TODO implement relations between other types
      _ -> throwError $ relationNotSupportedError p op t


  DS.EBool p op e1 e2 -> do
    (labelIf, labelElse, labelJoin) <- getIfElseLabels

    
    val1 <- getExprValue e1
    finishBlock $ CondBranch val1 labelIf labelElse
    
    case op of
      DS.And _ -> do
        newBlock labelIf
        val2 <- getExprValue e2
        finishBlock $ Branch labelJoin

        newBlock labelElse
        finishBlock $ Branch labelJoin

        newBlock labelJoin
        reg <- getNewRegDefault
        addInstr $ Ass reg $ Phi i1 [(labelIf, val2), (labelElse, false)]
        return $ Var reg

        
      DS.Or _ -> do
        newBlock labelIf
        finishBlock $ Branch labelJoin

        newBlock labelElse
        val2 <- getExprValue e2
        finishBlock $ Branch labelJoin

        newBlock labelJoin
        reg <- getNewRegDefault
        addInstr $ Ass reg $ Phi i1 [(labelIf, true), (labelElse, val2)]
        return $ Var reg
    
  ---------------------------------------------------------------------
  DS.NewArr   p t e        -> throwTODOP p
  DS.NewObj   p t          -> throwTODOP p
  DS.Cast     p t e        -> throwTODOP p
  DS.Concat   p e1 e2      -> do
    v1 <- getExprValue e1
    v2 <- getExprValue e2
    reg <- getNewRegDefault

    let singT = sing @(Ptr (I 8))
    let singTs = SCons singT $ SCons singT SNil
    let args = v1 :> v2 :> DNil
    addInstr $ Ass reg $ Call singT strConcatLabel singTs args
    return $ Var reg

getBinOp :: DS.BinOp -> BinOp (I n)
getBinOp op = case op of
  DS.Plus  p -> ADD
  DS.Minus p -> SUB
  DS.Times p -> MUL
  DS.Div   p -> SDIV
  DS.Mod   p -> SREM

getCMPKind :: DS.RelOp t -> CMPKind
getCMPKind op = case op of
  DS.LTH _ -> SLT
  DS.LE  _ -> SLE
  DS.GTH _ -> SGT
  DS.GE  _ -> SGE
  DS.EQU _ -> EQ
  DS.NE  _ -> NE


getArgs :: (MonadState LLVMState m, MonadError Error m)
  => SList ts
  -> DS.ExprList ts
  -> m (SList (GetPrimTypes ts), ArgList (GetPrimTypes ts))
getArgs SNil DNil = return (SNil, DNil)
getArgs (SCons t ts) (arg :> args) = do
  v <- getExprValue arg
  (tts, vs) <- getArgs ts args
  return (SCons (sGetPrimType t) tts, v :> vs)
