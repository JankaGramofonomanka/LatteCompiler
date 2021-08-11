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


import Data.Singletons.Sigma

import Control.Monad.State
import Control.Monad.Except

import Data.Singletons

import LLVM.LLVM
import LLVM.State
import LLVM.LLVMGetters
import LLVM.TypeConversion


import qualified Syntax.SyntaxDep as DS
import Position.Position
import Errors


addStmt :: (MonadState LLVMState m, MonadError Error m) => DS.Stmt -> m ()
addStmt stmt = case stmt of
  DS.Empty    p         -> return ()
  DS.BStmt    p bl      -> throwTODOP p
  DS.Decl     p t items -> do
    foldl (\m item -> m >> declareItem t item) (pure ()) items
    
  DS.Ass      p singT var expr -> do
    val <- getExprValue expr
    overwriteVar singT var val

  DS.Incr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation ADD val (ILit 1)

  DS.Decr     p var -> do
    val <- getVarValue DS.STInt var
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation SUB val (ILit 1)

  DS.Ret      p singT expr -> do
    v <- getExprValue expr
    finishBlock $ Ret (sGetPrimType singT :&: v)

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
      assignValue l singT x v


overwriteVar :: (MonadState LLVMState m, MonadError Error m)
  => Sing t -> DS.Var t -> Value (GetPrimType t) -> m ()
overwriteVar singT var val = case var of
  DS.Var p x -> do
    l <- getCurrentBlockLabel
    assignValue l singT x val

  DS.Attr {} -> throwTODOP (position var)
  DS.Elem {} -> throwTODOP (position var)
  DS.Null {} -> throwTODOP (position var)
  DS.Self {} -> throwTODOP (position var)

