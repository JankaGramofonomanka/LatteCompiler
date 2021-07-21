{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}

module TypeCheck.PuttersAndDeclarations where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import qualified Syntax.Syntax as S
import Syntax.SyntaxGADT
import Position.Position
import Errors
import LangElemClasses
import Syntax.Debloater
import qualified Scope as Sc
import TypeCheck.State
import TypeCheck.Getters


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
  put $ TypeCheckState { returnType = Just $ anyType t, .. }

dropReturnType :: (MonadState TypeCheckState m) => m ()
dropReturnType = do
  TypeCheckState { returnType = _, .. } <- get
  put $ TypeCheckState { returnType = Nothing, .. }

putSlefType :: (MonadState TypeCheckState m) => Type Custom -> m ()
putSlefType t = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Just t, .. }

dropSlefType :: (MonadState TypeCheckState m) => m ()
dropSlefType = do
  TypeCheckState { selfType = _, .. } <- get
  put $ TypeCheckState { selfType = Nothing, .. }


-------------------------------------------------------------------------------
declareId ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType t,
    HasPosition t
  )
  => t -> S.Ident -> m ()
declareId t id = case anyType t of
  AnyT tt -> do
    
    when (isVoid t) $ throwError $ voidDeclarationError (position t) id

    varScope <- gets varScope
    
    let varInfo = VarInfo (debloat id) tt
    case Sc.insertNew (name id) varInfo varScope of
      Nothing -> do
        VarInfo { varId = declared, .. } <- getIdentInfo id
        let declaredAt = position declared
        throwError $ varAlredyDeclaredError (position id) id declaredAt
      
      Just newScope -> putVarScope newScope


declareFunc ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType retType,
    IsType argType
  )
  => S.Ident -> retType -> [argType] -> m ()
declareFunc = declareCallable (Nothing :: Maybe S.Ident)

declareMethod ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType retType,
    IsType argType, 
    IsIdent i
  )
  => i -> S.Ident -> retType -> [argType] -> m ()
declareMethod i = declareCallable (Just i)
    
declareCallable ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType retType,
    IsType argType,
    IsIdent i
  )
  => Maybe i -> S.Ident -> retType -> [argType] -> m ()
declareCallable ownerCls id retType argTypes = case anyType retType of
  AnyT retT -> do
    
    fnScope <- gets funcScope
    let info = FuncInfo (debloat id) retT (map anyType argTypes)
    case Sc.insertNew (name id) info fnScope of
      Nothing -> do
        FuncInfo { funcId = declared, .. } <- getFuncInfo id
        let declaredAt = position declared
        case ownerCls of
          Just cls -> throwError
            $ methodAlredyDeclaredError (position id) id cls declaredAt
          Nothing -> throwError
            $ funcAlredyDeclaredError (position id) id declaredAt
      
      Just newScope -> putFuncScope newScope


declareClass :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> Maybe S.Ident -> S.ClassBody -> m ()
declareClass id maybeParent body = do
  clsMap <- gets classMap

  case M.lookup (name id) clsMap of
    Just ClassInfo { classId = c, .. } -> throwError
      $ classAlredyDeclaredError (position id) id (position c)
    
    Nothing -> do
      parentInfo <- getParentInfo maybeParent
      attrMap <- getAttrMap id body
      methodMap <- getMethodMap id body

      let clsInfo = ClassInfo (debloat id) parentInfo attrMap methodMap
      let newClsMap = M.insert (name id) clsInfo clsMap

      putClassMap newClsMap
      

  where

    getParentInfo maybeParent = do
      clsMap <- gets classMap

      case maybeParent of
        Just id -> case M.lookup (name id) clsMap of
          Nothing -> throwError $ noSuchClassError (position id) id
          Just info -> return $ Just info
        
        Nothing -> return Nothing


getAttrMap :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> S.ClassBody -> m VarMap
getAttrMap clsId (S.ClassBody _ memberDecls)
  = foldl addAttr (pure M.empty) memberDecls

  where
    addAttr :: MonadError Error m => m VarMap -> S.MemberDecl -> m VarMap
    addAttr acc (S.MethodDecl _) = acc
    addAttr acc (S.AttrDecl p t id) = do
      attrMap <- acc

      case M.lookup (name id) attrMap of
        Just VarInfo { varId = x, .. } -> 
          throwError $ attrAlredyDeclaredError p id clsId (position x)
          
        Nothing -> case anyType t of
          AnyT tt -> do
            let info = VarInfo (debloat id) tt
            return $ M.insert (name id) info attrMap


getMethodMap :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> S.ClassBody -> m FuncMap
getMethodMap clsId (S.ClassBody p memberDecls)
  = foldl addMethod (pure M.empty) memberDecls

  where
    addMethod :: MonadError Error m => m FuncMap -> S.MemberDecl -> m FuncMap
    addMethod acc (S.AttrDecl p t id) = acc
    addMethod acc (S.MethodDecl (S.ClassDef p _ _ _))
      = throwError $ nestedClassError p

    addMethod acc (S.MethodDecl (S.FnDef p retT id params body)) = do
      methodMap <- acc

      case M.lookup (name id) methodMap of
        Just FuncInfo { funcId = f, .. } -> 
          throwError $ methodAlredyDeclaredError p id clsId (position f)
          
        Nothing -> case anyType retT of
          AnyT retType -> do
            let paramTypes = map (anyType . typeOfParam) params
            let info = FuncInfo (debloat id) retType paramTypes
            return $ M.insert (name id) info methodMap

typeOfParam :: S.Param -> S.Type
typeOfParam (S.Param t _) = t

