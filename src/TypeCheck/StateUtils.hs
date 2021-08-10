{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
  , DataKinds
  , RankNTypes
#-}


module TypeCheck.StateUtils where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Unsafe.Coerce

import Data.Singletons
import Data.Singletons.Sigma

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors
import LangElemClasses
import Syntax.Debloater
import qualified Scope as Sc
import TypeCheck.State

import Dependent
import SingChar


mkClsId :: Pos -> SStr s -> ClassIdent s
mkClsId = ClassIdent


filterT :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> Error -> SLatteType a -> SLatteType b -> e b -> m (e a)
filterT _ _ STInt   STInt   x = return x
filterT _ _ STStr   STStr   x = return x
filterT _ _ STBool  STBool  x = return x
filterT p err (SArr t1) (SArr t2) x = do
  xx <- filterT p err t1 t2 (extractParam2 x)
  
  return $ insertParam2 xx

filterT p err (SCustom cls1) (SCustom cls2) x = do
  
  assertSubClass err (mkClsId p cls1) (mkClsId p cls2)
  return $ unsafeCoerce x


filterT p err expected actual x = throwError err




getIdentInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m VarInfo
getIdentInfo id = do
  varScope <- gets varScope
  case Sc.lookup (name id) varScope of
    Nothing   -> throwError $ noSuchVarError (position id) id
    Just info -> return info

getIdent :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => SLatteType a -> i -> m (Ident a)
getIdent expT id = do
  (VarInfo x actT _) <- getIdentInfo id
  let err = wrongIdentTypeError (position id) id expT actT 
  filterT (position id) err expT actT x


getFuncInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncInfo
getFuncInfo id = do
  fnScope <- gets funcScope
  case Sc.lookup (name id) fnScope of
    Nothing   -> throwError $ noSuchFuncError (position id) id
    Just info -> return info
    
getClassInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m ClassInfo
getClassInfo id = do
  clsMap <- gets classMap
  case M.lookup (name id) clsMap of
    Nothing   -> throwError $ noSuchClassError (position id) id
    Just info -> return info


getSelfType ::
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => Pos -> m (Sigma Str (TyCon1 (ExtractParam2 SLatteType Custom)))
getSelfType p = do
  maybeSelfType <- gets selfType
  case maybeSelfType of
    Nothing -> throwError $ selfOutsideClassError p
    Just t -> return t

getTypeKW :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SLatteType t -> m (TypeKW t)
getTypeKW kwPos t = case t of
  STInt       -> return $ KWInt kwPos
  STStr       -> return $ KWStr kwPos
  STBool      -> return $ KWBool kwPos
  STVoid      -> return $ KWVoid kwPos
  SArr elemT  -> do
    elemTT <- getTypeKW kwPos elemT
    return $ KWArr elemTT

  SCustom cls -> do
    
    let clsId = ClassIdent kwPos cls
    return $ KWCustom clsId
  
  STNull       -> throwError $ internallNullKWError kwPos

getSomeTypeKW
  :: (MonadState TypeCheckState m, MonadError Error m, IsType t, HasPosition t)
  => t -> m (Sigma LatteType (TyCon1 TypeKW))
getSomeTypeKW t = case someType t of
  Some tt -> do
    kw <- getTypeKW (position t) tt
    return $ tt :&: kw
  

{-
getClassIdent :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SStr s -> m (ClassIdent cls)
getClassIdent idPos cls = do
  let clsName = singToString cls
  let clsId = ClassIdent idPos clsName
  return clsId
-- -}

-------------------------------------------------------------------------------
isParent :: (MonadState TypeCheckState m, MonadError Error m)
  => ClassIdent a -> ClassIdent b -> m Bool
isParent cls1 cls2 = do
  if name cls1 == name cls2 then
    return True
  else do
    ClassInfo { parent = mbParent, .. } <- getClassInfo cls2
    case mbParent of
      Nothing -> return False
      Just ClassInfo { classId = parentId, .. } -> isParent cls1 parentId


assertSubClass :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> ClassIdent a -> ClassIdent b -> m ()
assertSubClass err parent child = do
  success <- isParent parent child
  if success then
    return ()
  else throwError err

assertRetTypeIsSomething :: 
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => Pos -> m ()
assertRetTypeIsSomething p = do
  maybeRetType <- gets returnType
  when (isNothing maybeRetType) $ throwError $ internalNoReturnTypeError p

getCommonType :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> Error -> Error -> SLatteType a -> SLatteType b -> m (Some SLatteType)
getCommonType p err errCls t1 t2 = case (t1, t2) of
  (SCustom cls1, SCustom cls2) -> do

    -- TODO fakePos potentially misleading in case of bugs
    cls1IsParent <- isParent (mkId cls1) (mkId cls2)
    cls2IsParent <- isParent (mkId cls2) (mkId cls1)

    if cls1IsParent then
      return $ Some t1
    else if cls2IsParent then
      return $ Some t2
    else
      throwError errCls
  
  _ -> do
    t <- filterT p err t1 t2 t2
    return $ Some t

  where
    mkId = mkClsId fakePos



-------------------------------------------------------------------------------
putVarScope :: MonadState TypeCheckState m => VarScope -> m ()
putVarScope scope = do
  TypeCheckState { varScope = _, .. } <- get
  put $ TypeCheckState { varScope = scope, .. }

putFuncScope :: MonadState TypeCheckState m => FuncScope -> m ()
putFuncScope fnScope = do
  TypeCheckState { funcScope = _, .. } <- get
  put $ TypeCheckState { funcScope = fnScope, .. }

putClassMap :: MonadState TypeCheckState m => ClassMap -> m ()
putClassMap clsMap = do
  TypeCheckState { classMap = _, .. } <- get
  put $ TypeCheckState { classMap = clsMap, .. }

updateVarScope :: MonadState TypeCheckState m => (VarScope -> VarScope) -> m ()
updateVarScope f = do
  TypeCheckState { varScope = scope, .. } <- get
  put $ TypeCheckState { varScope = f scope, .. }

subVarScope :: MonadState TypeCheckState m => m ()
subVarScope = updateVarScope Sc.subScope

dropVarScope :: MonadState TypeCheckState m => m ()
dropVarScope = updateVarScope Sc.dropScope

updateFuncScope :: MonadState TypeCheckState m
  => (FuncScope -> FuncScope) -> m ()
updateFuncScope f = do
  TypeCheckState { funcScope = scope, .. } <- get
  put $ TypeCheckState { funcScope = f scope, .. }

subFuncScope :: MonadState TypeCheckState m => m ()
subFuncScope = updateFuncScope Sc.subScope

dropFuncScope :: MonadState TypeCheckState m => m ()
dropFuncScope = updateFuncScope Sc.dropScope

putReturnType :: (MonadState TypeCheckState m, IsType t) => t -> m ()
putReturnType t = do
  TypeCheckState { returnType = _, .. } <- get
  put $ TypeCheckState { returnType = Just $ someType t, .. }

dropReturnType :: (MonadState TypeCheckState m) => m ()
dropReturnType = do
  TypeCheckState { returnType = _, .. } <- get
  put $ TypeCheckState { returnType = Nothing, .. }

putSlefType :: (MonadState TypeCheckState m) => SLatteType (Custom cls) -> m ()
putSlefType t@(SCustom s) = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Just (s :&: extractParam2 t), .. }

dropSlefType :: (MonadState TypeCheckState m) => m ()
dropSlefType = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Nothing, .. }




