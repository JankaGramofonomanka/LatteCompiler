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
    
  DS.Cond     p expr stm            -> throwTODOP p
  DS.CondElse p expr stmIf stmElse  -> throwTODOP p
  DS.While    p expr stm            -> throwTODOP p
  DS.SExp     p singT expr          -> throwTODOP p
  DS.For      p t i var stm         -> throwTODOP p


declareItem :: (MonadState LLVMState m, MonadError Error m)
  => DS.TypeKW t -> DS.Item t -> m ()
declareItem kw (DS.NoInit x) = do
  v <- getDefaultValue kw
  let singT = DS.singFromKW kw
  declareId singT x v

declareItem kw (DS.Init x e) = do
  v <- getExprValue e
  let singT = DS.singFromKW kw
  declareId singT x v


overwriteVar :: (MonadState LLVMState m, MonadError Error m)
  => Sing t -> DS.Var t -> Value (GetPrimType t) -> m ()
overwriteVar singT var val = case var of
  DS.Var p x -> overwriteId singT x val

  DS.Attr {} -> throwTODOP (position var)
  DS.Elem {} -> throwTODOP (position var)
  DS.Null {} -> throwTODOP (position var)
  DS.Self {} -> throwTODOP (position var)

