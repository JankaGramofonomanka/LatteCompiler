{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
  , DataKinds
#-}

module TypeCheckDep.StateUtils where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Prelude hiding ( Error, Any )

import Data.Maybe
import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors ( Error )
import GenErrors
import LangElemClasses
import qualified Scope as Sc
import TypeCheckDep.State
import Dependent







updatePos ::
  ( MonadState TypeCheckState m
  , MonadError Error m
  , HasPosition a
  )
  => a -> m ()
updatePos x = do
  TypeCheckState { currentPos = _, .. } <- get
  put $ TypeCheckState { currentPos = position x, .. }

updatePosTemp :: 
  ( MonadState TypeCheckState m
  , MonadError Error m
  , HasPosition a
  )
  => a -> m b ->  m b
updatePosTemp x y = do
  pos <- gets currentPos
  updatePos x
  result <- y
  updatePos pos
  return result

  


type TPError t = TypeKW t -> Pos -> Error
type TTPError t1 t2 = TypeKW t1 -> TypeKW t2 -> Pos -> Error


throwPError :: (MonadState TypeCheckState m, MonadError Error m)
  => PError -> m a
throwPError err = do
  pos <- gets currentPos
  throwError $ err pos


throwTPError :: (MonadState TypeCheckState m, MonadError Error m)
  => TPError t -> SLatteType t -> m a
throwTPError err t = do
  kwT <- getTypeKW fakePos t
  throwPError $ err kwT

throwTTPError :: (MonadState TypeCheckState m, MonadError Error m)
  => TTPError t1 t2 -> SLatteType t1 -> SLatteType t2 -> m a
throwTTPError err t1 t2 = do
  kwT1 <- getTypeKW fakePos t1
  kwT2 <- getTypeKW fakePos t2
  throwPError $ err kwT1 kwT2

throwTODO :: (MonadState TypeCheckState m, MonadError Error m)
  => m a
throwTODO = throwPError todoError




someType :: (MonadState TypeCheckState m, MonadError Error m, IsType t)
  => t -> m (Some SLatteType)
someType t = case toSimpleType t of
  S.Int  p    -> return $ Some STInt
  S.Str  p    -> return $ Some STStr
  S.Bool p    -> return $ Some STBool
  S.Void p    -> return $ Some STVoid
  S.Arr elemT -> do
    Some elemTT <- someType elemT
    return $ Some $ SArr elemTT

  S.Custom id -> do
    ClassInfo { classId = clsId, .. } <- getClassInfo id
    return $ Some $ SCustom clsId




filterT :: (MonadState TypeCheckState m, MonadError Error m)
  => TTPError a b -> SLatteType a -> SLatteType b -> e b -> m (e a)
filterT _ STInt   STInt   x = return x
filterT _ STStr   STStr   x = return x
filterT _ STBool  STBool  x = return x
filterT err (SArr t1) (SArr t2) x = do
  let err' = (\tt1 tt2 -> err (KWArr tt1) (KWArr tt2))
  xx <- filterT err' t1 t2 (extractParam2 x)
  
  return $ insertParam2 xx

filterT err (SCustom id1) (SCustom id2) x = do
  
  --assertSubClass err id1 id2
  --return $ forceCast x

  -- TODO no auto casting of subclasses
  xx <- filterNatural err id1 id2 (extractParam2 x)
  return $ insertParam2 xx

  where

    filterNatural 
      :: (MonadState TypeCheckState m, MonadError Error m)
      => TTPError (Custom a) (Custom b)
      -> SNatural a
      -> SNatural b
      -> e b
      -> m (e a)
    filterNatural _ SZero SZero x = return x
    filterNatural err (SSucc n1) (SSucc n2) x = do

      let err' = force err
      xx <- filterNatural err' n1 n2 (extractParam2 x)
      return $ insertParam2 xx

    filterNatural err t1 t2 _ = throwTTPError err (SCustom t1) (SCustom t2)
    
    force 
      :: TTPError (Custom (Succ a)) (Custom (Succ b))
      -> TTPError (Custom a) (Custom b)
    force err = \kw1 kw2 -> err (forceKW kw1) (forceKW kw2) where

      forceKW :: TypeKW (Custom a) -> TypeKW (Custom (Succ a))
      forceKW (KWCustom (ClassIdent p s)) = KWCustom (ClassIdent p s)

filterT err expected actual x = throwTTPError err expected actual




 -- getters -------------------------------------------------------------------
getIdentInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m VarInfo
getIdentInfo id = updatePosTemp id $ do
  varScope <- gets varScope
  case Sc.lookup (name id) varScope of
    Nothing   -> throwPError $ noSuchVarError id
    Just info -> return info

getIdent :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => SLatteType a -> i -> m (Ident a)
getIdent expT id = updatePosTemp id $ do
  (VarInfo x actT) <- getIdentInfo id

  let err = wrongIdentTypeError id
  filterT err expT actT x


getFuncInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncInfo
getFuncInfo id = updatePosTemp id $ do
  fnScope <- gets funcScope
  case Sc.lookup (name id) fnScope of
    Nothing   -> throwPError $ noSuchFuncError id
    Just info -> return info
    
getClassInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m ClassInfo
getClassInfo id = updatePosTemp id $ do
  clsMap <- gets classMap
  case M.lookup (name id) clsMap of
    Nothing   -> throwPError $ noSuchClassError id
    Just info -> return info

getClassIdent :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SClassId cls -> m (ClassIdent cls)
getClassIdent p cls = do
  m <- gets classNameMap
  case M.lookup (fromSing cls) m of
    Nothing -> throwPError internalNoClassError
    Just s -> return $ ClassIdent p s

getTypeKW :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SLatteType t -> m (TypeKW t)
getTypeKW p t = case t of
  STInt       -> return $ KWInt p
  STStr       -> return $ KWStr p
  STBool      -> return $ KWBool p
  STVoid      -> return $ KWVoid p
  SArr elemT  -> do
    elemTT <- getTypeKW p elemT
    return $ KWArr elemTT

  SCustom cls -> do
    clsId <- getClassIdent p cls
    return $ KWCustom clsId
  
  STNull       -> throwPError internallNullKWError

someTypeKW :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Type -> m (Some TypeKW)
someTypeKW p t = do
  Some tt <- someType t
  typeKW <- getTypeKW p tt
  return $ Some typeKW


{-
getFunc ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncIdent
getFunc id = do
  info <- getFuncInfo id
  return $ funcId info
-- -}

{-
getClass ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m ClassIdent
getClass id = do
  info <- getClassInfo id
  return $ classId info
-- -}

getSelfType ::
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => m SomeCustomType
getSelfType = do
  maybeSelfType <- gets selfType
  case maybeSelfType of
    Nothing -> throwPError selfOutsideClassError
    Just t -> return t

getNewClassId :: (MonadState TypeCheckState m) => m Natural
getNewClassId = do
  TypeCheckState { classCounter = n, .. } <- get
  put $ TypeCheckState { classCounter = Succ n, .. }
  return n

getSomeNewClassId :: (MonadState TypeCheckState m) => m (Some SClassId)
getSomeNewClassId = do
  TypeCheckState { classCounter = n, .. } <- get
  put $ TypeCheckState { classCounter = Succ n, .. }
  case toSing n of
    SomeSing n -> return $ Some n


-------------------------------------------------------------------------------

{-
isParent :: (MonadState TypeCheckState m, MonadError Error m)
  => SClassId cls1 -> SClassId cls2 -> m Bool
isParent cls1 cls2 = do
  if cls1 == cls2 then
    return True
  else do
    ClassInfo { parent = mbParent, .. } <- getClassInfo cls2
    case mbParent of
      Nothing -> return False
      Just (ClassInfo parentId _ _ _) -> isParent cls1 parentId


assertSubClass :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> SClassId cls1 -> SClassId cls2 -> m ()
assertSubClass err parent child = do
  success <- isParent parent child
  if success then
    return ()
  else throwPError err


getCommonType :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> Error -> SLatteType a -> SLatteType b -> m (Some SLatteType)
getCommonType err errCls t1 t2 = case (t1, t2) of
  (SCustom cls1, SCustom cls2) -> do
    cls1IsParent <- isParent cls1 cls2
    cls2IsParent <- isParent cls2 cls1

    if cls1IsParent then
      return $ Some t1
    else if cls2IsParent then
      return $ Some t2
    else
      throwPError errCls
  
  _ -> do
    t <- filterT err t1 t2 t2
    return $ Some t
-- -}

assertRetTypeIsSomething :: 
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => m ()
assertRetTypeIsSomething = do
  maybeRetType <- gets returnType
  when (isNothing maybeRetType) $ throwPError internalNoReturnTypeError


-- putters --------------------------------------------------------------------
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

putReturnType :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Type -> m ()
putReturnType t = do
  someT <- someType t
  TypeCheckState { returnType = _, .. } <- get
  put $ TypeCheckState { returnType = Just someT, .. }

dropReturnType :: (MonadState TypeCheckState m) => m ()
dropReturnType = do
  TypeCheckState { returnType = _, .. } <- get
  put $ TypeCheckState { returnType = Nothing, .. }

putSlefType :: (MonadState TypeCheckState m) => SomeCustomType -> m ()
putSlefType t = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Just t, .. }

dropSlefType :: (MonadState TypeCheckState m) => m ()
dropSlefType = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Nothing, .. }







