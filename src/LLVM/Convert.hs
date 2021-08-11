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

module LLVM.Convert where

import qualified Data.Map as M

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error )

import Control.Monad.State
import Control.Monad.Except

import Data.Singletons

import LLVM.LLVM
import LLVM.State
import LLVM.StateUtils
import LLVM.LLVMGetters
import LLVM.TypeConversion


import qualified Syntax.SyntaxDep as DS
import Position.Position
import LangElemClasses
import Errors
import Dependent
import BuiltIns


addStmt :: (MonadState LLVMState m, MonadError Error m) => DS.Stmt -> m ()
addStmt stmt = case stmt of
  DS.Empty    p -> return ()
  DS.BStmt    p (DS.Block _ stmts) -> do
    
    enter <- getNewLabel "EnterSubScope"
    exit <- getNewLabel "ExitSubScope"
    
    finishBlock $ Branch enter
    incrScopeLevel
    newBlock enter

    mapM_ addStmt stmts

    finishBlock $ Branch exit
    decrScopeLevel 
    newBlock exit    

  DS.Decl     p t items -> do
    foldl (\m item -> m >> declareItem t item) (pure ()) items
    
  DS.Ass      p singT var expr -> do
    val <- getExprValue expr
    overwriteVar singT var val

  DS.Incr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 ADD val (ILit 1)

  DS.Decr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation i32 SUB val (ILit 1)

  DS.Ret      p singT expr -> do
    v <- getExprValue expr
    finishBlock $ Ret (sGetPrimType singT) v

  DS.VRet     p -> do
    finishBlock RetVoid

  DS.Cond     p expr stm -> do
    (labelIf, _,  labelJoin) <- getIfElseLabels
    cond <- getExprValue expr
    finishBlock $ CondBranch cond labelIf labelJoin

    newBlock labelIf
    addStmt stm
    finishBlock $ Branch labelJoin

    newBlock labelJoin

  DS.CondElse p expr stmIf stmElse -> do
    (labelIf, labelElse,  labelJoin) <- getIfElseLabels
    cond <- getExprValue expr
    finishBlock $ CondBranch cond labelIf labelElse

    newBlock labelIf
    addStmt stmIf
    finishBlock $ Branch labelJoin

    newBlock labelElse
    addStmt stmElse
    finishBlock $ Branch labelJoin

    newBlock labelJoin

  DS.While p expr stm -> do
    (labelCond, labelLoop, labelJoin) <- getWhileLabels
    
    finishBlock $ Branch labelCond

    newBlock labelLoop
    addStmt stm
    finishBlock $ Branch labelCond

    newBlock labelCond
    cond <- getExprValue expr
    finishBlock $ CondBranch cond labelLoop labelJoin

    newBlock labelJoin

  DS.SExp p singT expr -> do
    _ <- getExprValue expr
    return ()

  DS.For p t i var stm -> throwTODOP p


declareItem :: (MonadState LLVMState m, MonadError Error m)
  => DS.TypeKW t -> DS.Item t -> m ()
declareItem kw it = case it of
  (DS.NoInit x) -> getDefaultValue kw >>= declIt kw x

  (DS.Init x e) -> getExprValue e >>= declIt kw x

  where
    declIt kw x v = do
      let singT = DS.singFromKW kw
      l <- getCurrentBlockLabel
      assignValue l (typedIdent singT x) v


overwriteVar :: (MonadState LLVMState m, MonadError Error m)
  => Sing t -> DS.Var t -> Value (GetPrimType t) -> m ()
overwriteVar singT var val = case var of
  DS.Var p x -> do
    l <- getCurrentBlockLabel
    assignValue l (typedIdent singT x) val

  DS.Attr {} -> throwTODOP (position var)
  DS.Elem {} -> throwTODOP (position var)
  DS.Null {} -> throwTODOP (position var)
  DS.Self {} -> throwTODOP (position var)


-------------------------------------------------------------------------------
addFnDef :: (MonadState LLVMState m, MonadError Error m) => DS.FnDef -> m ()
addFnDef (DS.FnDef p t funcId params (DS.Block _ stmts)) = do
  incrScopeLevel

  (primTs, primArgs) <- getParams params
  let currentFunc = PotFunc { label = FuncLabel (name funcId)
                            , retType = DS.singFromKW t
                            , argTypes = primTs
                            , args = primArgs
                            , body = M.empty
                            }

  putCurrentFunc currentFunc

  l <- getNewLabel (name funcId)
  newBlock l
  declareParams l primArgs params

  mapM_ addStmt stmts
  finishFunc p

  decrScopeLevel
  throwTODOP p

getParams :: (MonadState LLVMState m, MonadError Error m)
 => DS.ParamList ts
 -> m (SList (GetPrimTypes ts), ParamList (GetPrimTypes ts))
getParams DNil = return (SNil, DNil)
getParams (DS.Param kw x :> params) = do
  (types, args) <- getParams params
  reg <- getNewReg (name x)
  return (SCons (sGetPrimType $ DS.singFromKW kw) types, reg :> args)

declareParams :: (MonadState LLVMState m, MonadError Error m)
  => Label -> ParamList (GetPrimTypes ts) -> DS.ParamList ts -> m ()
declareParams _ DNil DNil = return ()
declareParams l (reg :> regs) (DS.Param kw x :> params) = do
  let singT = DS.singFromKW kw
  let typedX = typedIdent singT x
  assignValue l typedX (Var reg)


-------------------------------------------------------------------------------
addProg :: (MonadState LLVMState m, MonadError Error m) => DS.Program -> m ()
addProg (DS.Program _ defs) = mapM_ addFnDef' defs where
  addFnDef' (Left def) = throwTODOP (position def)
  addFnDef' (Right def) = addFnDef def


extractLLVM :: (MonadState LLVMState m, MonadError Error m) => m LLVMProg
extractLLVM = do
  PotProg { mainFunc = mbMain, funcs = funcs, .. } <- gets currentProg
  case mbMain of
    Nothing -> throwError noMainError
    Just main -> do

      strConstnts <- gets $ map (fromSomeStrConst . snd) . M.toList . strLitMap

      let prog = LLVM { mainFunc = main
                      , funcs = funcs
                      , externFuncs = externFuncLabels
                      , constants = strConstnts
                      }

      return prog


fromSomeStrConst :: SomeStrConst -> SomeConstant
fromSomeStrConst (len :&: c) = SArray (sing @(I 8)) len :&: c



