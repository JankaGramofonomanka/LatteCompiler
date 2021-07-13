{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Step1.State where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import qualified Syntax.Syntax as A
import Syntax.SyntaxGADT
import Position
import Errors
import Syntax.SyntaxPosition
import LangElemClasses

type VarMap = M.Map String (Any Ident)
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo

data FuncInfo = FuncInfo {
  funcId :: FuncIdent,
  retType :: Any Type,
  paramTypes :: [Any Type]
}

data ClassInfo = ClassInfo {
  classId :: ClassIdent,
  parent :: Maybe ClassInfo,
  attributes :: VarMap,
  methods :: FuncMap
}

data Step1State = Step1State { 
  varMap :: VarMap,
  funcMap :: FuncMap,
  classMap :: ClassMap
}



getVarInt :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m (Ident Int)
getVarInt id = do
  varMap <- gets varMap
  case M.lookup (name id) varMap of
    Nothing -> throwError $ noSuchVarError (position id) id
    Just (Any (Int _) x)  -> return x
    Just (Any t x)        -> throwError 
                              $ wrongTypeError (position id) id int t

  where
    int = Int fakePos

getVarStr :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m (Ident String)
getVarStr id = do
  varMap <- gets varMap
  case M.lookup (name id) varMap of
    Nothing -> throwError $ noSuchVarError (position id) id
    Just (Any (Str _) x)  -> return x
    Just (Any t x)        -> throwError 
                              $ wrongTypeError (position id) id str t

  where
    str = Str fakePos

getVarBool :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m (Ident Bool)
getVarBool id = do
  varMap <- gets varMap
  case M.lookup (name id) varMap of
    Nothing -> throwError $ noSuchVarError (position id) id
    Just (Any (Bool _) x) -> return x
    Just (Any t x)        -> throwError 
                              $ wrongTypeError (position id) id bool t

  where
    bool = Bool fakePos


getFuncInfo :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m FuncInfo
getFuncInfo id = do
  varMap <- gets funcMap
  case M.lookup (name id) varMap of
    Nothing   -> throwError $ noSuchFuncError (position id) id
    Just info -> return info
    
getClassInfo :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m ClassInfo
getClassInfo id = do
  varMap <- gets classMap
  case M.lookup (name id) varMap of
    Nothing   -> throwError $ noSuchClassError (position id) id
    Just info -> return info

getFunc :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m FuncIdent
getFunc id = do
  info <- getFuncInfo id
  return $ funcId info
    
getClass :: (MonadState Step1State m, MonadError Error m)
  => A.Ident -> m ClassIdent
getClass id = do
  info <- getClassInfo id
  return $ classId info


