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
import qualified Constants as C

import Dependent
import BuiltIns



true, false :: Value ('I 1)
true = BoolLit True
false = BoolLit False

-------------------------------------------------------------------------------
getAttrPtr :: LLVMConverter m
  => Sing (GetPrimType t)
  -> Sing (Struct s)
  -> Value (Ptr (Struct s))
  -> DS.Ident t
  -> m (Reg (Ptr (GetPrimType t)))
getAttrPtr attrT clsT clsV attrId = do

  i <- getAttrNumber clsT attrId

  attrPtr <- getNewReg $ name attrId ++ C.ptrPostfix
  addInstr $ Ass attrPtr $ GetAttrPtr clsT i32 i32 clsV (ILit 0) (ILit i)

  return attrPtr


getArrPtr :: LLVMConverter m
  => Sing t -> Value (Ptr (ArrStruct t)) -> m (Reg (Ptr (Ptr t)))
getArrPtr elemT arrV = do
  let arrT = SArrStruct elemT

  arrPtr <- getNewReg C.regArrPtr
  addInstr $ Ass arrPtr $ GetArrAttrPtr arrT i32 i32 arrV (ILit 0) (ILit 0)
  
  return arrPtr

getElemPtr :: LLVMConverter m
  => Sing t -> Value (Ptr (ArrStruct t)) -> Value (I 32) -> m (Reg (Ptr t))
getElemPtr elemT arrV index = do
  arrPtr <- getArrPtr elemT arrV
  arr <- getNewReg C.regArr
  addInstr $ Ass arr $ Load (SPtr elemT) (Var arrPtr)

  elemPtr <- getNewReg C.regElemPtr
  addInstr $ Ass elemPtr $ GetElemPtr elemT i32 (Var arr) index

  return elemPtr

getArrLengthPtr :: LLVMConverter m
  => Sing (ArrStruct t) -> Value (Ptr (ArrStruct t)) -> m (Reg (Ptr ('I 32)))
getArrLengthPtr arrT arrV = do
  lengthPtr <- getNewReg C.lengthAttrPtr

  addInstr $ Ass lengthPtr $ GetArrAttrPtr arrT i32 i32 arrV (ILit 0) (ILit 1)

  return lengthPtr

getElem :: LLVMConverter m
  => Sing t -> Value (Ptr (ArrStruct t)) -> Value (I 32) -> m (Value t)
getElem elemT arrV index = do
  elemPtr <- getElemPtr elemT arrV index
  elem <- getNewReg C.regElem
  addInstr $ Ass elem $ Load elemT (Var elemPtr)
  return (Var elem)

getArrLength :: LLVMConverter m
  => Sing (ArrStruct t) -> Value (Ptr (ArrStruct t)) -> m (Value ('I 32))
getArrLength arrT arrV = do
  lengthPtr <- getArrLengthPtr arrT arrV
  length <- getNewReg C.lengthAttr
  addInstr $ Ass length $ Load i32 (Var lengthPtr)

  return (Var length)


-------------------------------------------------------------------------------
getVarValue :: LLVMConverter m
  => Sing t -> DS.Var t -> m (Value (GetPrimType t))
getVarValue singT var = case var of
  DS.Var p x -> do
    l <- getCurrentBlockLabel
    getIdentValue (typedIdent singT x) l

  DS.Attr p cls e attrId -> do
    let SPtr clsT = sGetPrimType cls
    let attrT = sGetPrimType singT

    eVal <- getExprValue e
    attrPtr <- getAttrPtr attrT clsT eVal attrId
    attr <- getNewReg $ name attrId
    addInstr $ Ass attr $ Load attrT (Var attrPtr)

    return (Var attr)

  DS.Length p t e -> do
    let SPtr arrT = sGetPrimType t
    eVal <- getExprValue e
    
    getArrLength arrT eVal

  DS.Elem p e i -> do
    let elemT = sGetPrimType singT
    eVal <- getExprValue e
    iVal <- getExprValue i
    
    getElem elemT eVal iVal
    


  DS.Null   {} -> throwTODOP (position var)
  DS.Self   {} -> throwTODOP (position var)


-------------------------------------------------------------------------------
getExprValue :: LLVMConverter m => DS.Expr t -> m (Value (GetPrimType t))
getExprValue expr = case expr of
  DS.EVar p singT v -> getVarValue singT v

  ---------------------------------------------------------------------
  DS.ELitInt  p i -> return (ILit i)
  DS.ELitBool p b -> return (BoolLit b)
  DS.EString  p s -> do
    (n :&: ptr) <- getStrLitConstPtr s
    reg <- getNewRegDefault

    let singT = SArray (sing @(I 8)) n
    addInstr $ Ass reg $ GetArrElemPtr singT i32 i32 ptr (ILit 0) (ILit 0)
    return $ Var reg

  ---------------------------------------------------------------------
  DS.EApp p t f argTs args -> case f of
    DS.Func _ funcId -> do
      let funcLabel = FuncLabel (name funcId)
      (argTypes, argList) <- getArgs argTs args
      reg <- getNewRegDefault

      let singT = sGetPrimType t
      case singT of
        SVoid -> do
          addInstr $ VoidExpr $ Call singT funcLabel argTypes argList
          return $ Var reg

        _ -> do
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
    condBranch' (Var cond) labelIf labelElse
    
    newBlock labelIf
    branch' labelJoin
    newBlock labelElse
    branch' labelJoin
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
      DS.STInt -> getOpDefault t op e1 e2
      DS.STBool -> getOpDefault t op e1 e2
      DS.SCustom s -> getOpDefault t op e1 e2
      
      -- TODO implement relations between other types
      _ -> throwError $ relationNotSupportedError p op t

      where
        getOpDefault :: LLVMConverter m
          => Sing t
          -> DS.RelOp t
          -> DS.Expr t
          -> DS.Expr t
          -> m (Value (I 1))
          
        getOpDefault singT op ex1 ex2 = do
          v1 <- getExprValue ex1
          v2 <- getExprValue ex2
          let cmpKind = getCMPKind op
          reg <- getNewRegDefault
      
          addInstr $ Ass reg $ ICMP (sGetPrimType singT) cmpKind v1 v2
          return $ Var reg


  DS.EBool p op e1 e2 -> do
    (labelIf, labelElse, labelJoin) <- getIfElseLabels

    
    val1 <- getExprValue e1
    condBranch' val1 labelIf labelElse
    
    case op of
      DS.And _ -> do
        newBlock labelIf
        val2 <- getExprValue e2
        labelIfExit <- getCurrentBlockLabel
        branch' labelJoin

        newBlock labelElse
        branch' labelJoin

        newBlock labelJoin
        reg <- getNewRegDefault
        addInstr $ Ass reg $ Phi i1 [(labelIfExit, val2), (labelElse, false)]
        return $ Var reg

        
      DS.Or _ -> do
        newBlock labelIf
        branch' labelJoin

        newBlock labelElse
        val2 <- getExprValue e2
        labelElseExit <- getCurrentBlockLabel
        branch' labelJoin

        newBlock labelJoin
        reg <- getNewRegDefault
        addInstr $ Ass reg $ Phi i1 [(labelIf, true), (labelElseExit, val2)]
        return $ Var reg
    
  ---------------------------------------------------------------------
  DS.NewArr p t e -> do
    let elemT = sGetPrimType (DS.singFromKW t)
    eVal <- getExprValue e
    arr <- getNewReg C.regArrStruct

    let retT = SPtr (SArrStruct elemT)
    let argTs = SCons i32 SNil
    let args = eVal :> DNil
    addInstr $ Ass arr $ Call retT (newArrLabel elemT) argTs args

    addArrType elemT

    return (Var arr)

  DS.NewObj p t -> do
    let SPtr singT = sGetPrimType (DS.singFromKW t)
    reg <- getNewReg C.regObj

    let argTs = SCons i32 SNil
    let args = ILit 1 :> DNil
    addInstr $ Ass reg $ Call (SPtr singT) (mallocLabel singT) argTs args

    addMallocType singT

    return (Var reg)

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


getArgs :: LLVMConverter m
  => SList ts
  -> DS.ExprList ts
  -> m (SList (GetPrimTypes ts), ArgList (GetPrimTypes ts))
getArgs SNil DNil = return (SNil, DNil)
getArgs (SCons t ts) (arg :> args) = do
  v <- getExprValue arg
  (tts, vs) <- getArgs ts args
  return (SCons (sGetPrimType t) tts, v :> vs)
