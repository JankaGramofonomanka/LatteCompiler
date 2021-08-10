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



import Control.Monad.State
import Control.Monad.Except

import Data.Singletons

import LLVM.LLVM
import LLVM.State
import LLVM.LLVMGetters

import qualified Syntax.SyntaxDep as DS
import Errors


addStmt :: (MonadState LLVMState m, MonadError Error m) => DS.Stmt -> m ()
addStmt stmt = case stmt of
  DS.Empty    p                     -> return ()
  DS.BStmt    p bl                  -> throwTODOP p
  DS.Decl     p t items             -> do
    
    foldl (\m item -> m >> declareItem t item) (pure ()) items
    

  DS.Ass      p t expr              -> throwTODOP p
  DS.Incr     p var                 -> throwTODOP p
  DS.Decr     p var                 -> throwTODOP p
  DS.Ret      p expr                -> throwTODOP p
  DS.VRet     p                     -> throwTODOP p
  DS.Cond     p expr stm            -> throwTODOP p
  DS.CondElse p expr stmIf stmElse  -> throwTODOP p
  DS.While    p expr stm            -> throwTODOP p
  DS.SExp     p expr                -> throwTODOP p
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