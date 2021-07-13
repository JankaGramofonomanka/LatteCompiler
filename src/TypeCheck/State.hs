{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module TypeCheck.State where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import qualified Syntax.Syntax as A
import Syntax.SyntaxGADT
import Position
import Errors
import Syntax.SyntaxPosition
import LangElemClasses

type IdentMap = M.Map String IdentInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo

data IdentInfo where
  IdentInfo :: { varId :: Ident a, varType :: Type a } -> IdentInfo

data FuncInfo where
  FuncInfo :: {
    funcId :: FuncIdent,
    retType :: Type a,
    paramTypes :: [Any Type]
  } -> FuncInfo

data ClassInfo = ClassInfo {
  classId :: ClassIdent,
  parent :: Maybe ClassInfo,
  attributes :: IdentMap,
  methods :: FuncMap
}

data TypeCheckState = TypeCheckState { 
  idMap :: IdentMap,
  funcMap :: FuncMap,
  classMap :: ClassMap
}

newtype X1 (a :: * -> *) b = X1 (a b)
newtype X2 (a :: * -> *) (b :: * -> *) c = X2 (a (b c))
toX2 :: e (a b) -> X2 e a b
toX2 = X2

fromX2 :: X2 e a b -> e (a b)
fromX2 (X2 x) = x

filterT :: (MonadError Error m) => Error -> Type a -> Type b -> e b -> m (e a)
filterT _ (Int _)  (Int _)  x = return x
filterT _ (Str _)  (Str _)  x = return x
filterT _ (Bool _) (Bool _) x = return x
filterT err (Arr t1) (Arr t2) x = do
  xx <- filterT err t1 t2 (toX2 x)
  
  return $ fromX2 xx


filterT err (Custom id1) (Custom id2) x = do
  if id1 == id2 then
    return x
  else
    throwError err

filterT err expected actual x = throwError err

getIdentInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => A.Ident -> m IdentInfo
getIdentInfo id = do
  idMap <- gets idMap
  case M.lookup (name id) idMap of
    Nothing   -> throwError $ noSuchVarError (position id) id
    Just info -> return info

getIdent :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> A.Ident -> m (Ident a)
getIdent expT id = do
  (IdentInfo x actT) <- getIdentInfo id
  let err = wrongVarTypeError (position id) id expT actT 
  filterT err expT actT x


getFuncInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => A.Ident -> m FuncInfo
getFuncInfo id = do
  idMap <- gets funcMap
  case M.lookup (name id) idMap of
    Nothing   -> throwError $ noSuchFuncError (position id) id
    Just info -> return info
    
getClassInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => A.Ident -> m ClassInfo
getClassInfo id = do
  idMap <- gets classMap
  case M.lookup (name id) idMap of
    Nothing   -> throwError $ noSuchClassError (position id) id
    Just info -> return info

getFunc :: (MonadState TypeCheckState m, MonadError Error m)
  => A.Ident -> m FuncIdent
getFunc id = do
  info <- getFuncInfo id
  return $ funcId info
    
getClass :: (MonadState TypeCheckState m, MonadError Error m)
  => A.Ident -> m ClassIdent
getClass id = do
  info <- getClassInfo id
  return $ classId info

{-
getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> A.Var -> m (Var a)
getVar t var = case var of
  A.Var p id -> do
    x <- getIdent t id
    return $ Var p x

  A.Fun p id -> throwError $ notAVarError p id

  A.Member p v id -> do
    owner <- getVarClass v
    return x

  A.Elem p arr i -> do
    return x
-- -}
