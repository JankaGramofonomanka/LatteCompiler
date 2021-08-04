{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}

module TypeCheckDep.PuttersAndDeclarations where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Prelude hiding ( Error )
import Data.Singletons.Sigma

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors ( Error )
import GenErrors
import LangElemClasses
import Syntax.DebloaterDep
import qualified Scope as Sc
import TypeCheckDep.State
import TypeCheckDep.Getters
import Dependent

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



-------------------------------------------------------------------------------
declareId ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType t,
    HasPosition t
  )
  => t -> S.Ident -> m ()
declareId t id = updatePosTemp t $ do
    Some tt <- someType t
    
    when (isVoid t) $ throwPosError $ voidDeclarationError id

    varScope <- gets varScope
    
    let varInfo = VarInfo (debloat id) tt
    updatePosTemp id $ case Sc.insertNew (name id) varInfo varScope of
      Nothing -> do
        VarInfo { varId = declared, .. } <- getIdentInfo id
        let declaredAt = position declared
        throwPosError $ varAlredyDeclaredError id declaredAt
      
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


-- TODO how to update position
declareCallable ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType retType,
    IsType argType,
    IsIdent i
  )
  => Maybe i -> S.Ident -> retType -> [argType] -> m ()
declareCallable ownerCls funcId retType argTypes = do
    Some retT <- someType retType
    
    fnScope <- gets funcScope
    paramTypes :&: _ <- getTypeList id argTypes
    let info = FuncInfo (debloat funcId) retT paramTypes

    updatePosTemp funcId $ case Sc.insertNew (name funcId) info fnScope of

      Nothing -> do
        
        FuncInfo { funcId = declared, .. } <- getFuncInfo funcId
        let declaredAt = position declared
        
        case ownerCls of
          Just cls -> throwPosError
            $ methodAlredyDeclaredError funcId cls declaredAt
            
          Nothing -> throwPosError
            $ funcAlredyDeclaredError funcId declaredAt
      
      Just newScope -> putFuncScope newScope



declareClass :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> Maybe S.Ident -> S.ClassBody -> m ()
declareClass id maybeParent body = do
  clsMap <- gets classMap

  updatePosTemp id $ case M.lookup (name id) clsMap of
    Just ClassInfo { classId = c, declaredAt = p, .. } -> throwPosError
      $ classAlredyDeclaredError id p
    
    Nothing -> do
      parentInfo <- getParentInfo maybeParent
      attrMap <- getAttrMap id body
      methodMap <- getMethodMap id body

      Some clsId <- getSomeNewClassId
      let clsInfo = ClassInfo clsId parentInfo attrMap methodMap (position id)
      let newClsMap = M.insert (name id) clsInfo clsMap

      putClassMap newClsMap
      

  where

    getParentInfo maybeParent = do
      clsMap <- gets classMap

      case maybeParent of
        Just id -> case M.lookup (name id) clsMap of
          Nothing -> 
            updatePosTemp id $ throwPosError $ noSuchClassError id

          Just info -> return $ Just info
        
        Nothing -> return Nothing


getAttrMap :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> S.ClassBody -> m VarMap
getAttrMap clsId (S.ClassBody _ memberDecls)
  = foldl addAttr (pure M.empty) memberDecls

  where
    addAttr :: (MonadState TypeCheckState m, MonadError Error m)
      => m VarMap -> S.MemberDecl -> m VarMap
    addAttr acc (S.MethodDecl _) = acc
    addAttr acc (S.AttrDecl p t id) = updatePosTemp p $ do
      attrMap <- acc

      case M.lookup (name id) attrMap of
        Just VarInfo { varId = x, .. } -> 
          throwPosError $ attrAlredyDeclaredError id clsId (position x)
          
        Nothing -> do
          Some tt <- someType t
          let info = VarInfo (debloat id) tt
          return $ M.insert (name id) info attrMap


getMethodMap :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> S.ClassBody -> m FuncMap
getMethodMap clsId (S.ClassBody p memberDecls)
  = foldl addMethod (pure M.empty) memberDecls

  where
    addMethod :: (MonadState TypeCheckState m, MonadError Error m)
      => m FuncMap -> S.MemberDecl -> m FuncMap
    addMethod acc (S.AttrDecl p t id) = acc
    addMethod acc (S.MethodDecl (S.ClassDef p _ _ _))
      = updatePosTemp p $ throwPosError nestedClassError

    addMethod acc (S.MethodDecl (S.FnDef p retT id params body))
      = updatePosTemp p $ do
        methodMap <- acc

        case M.lookup (name id) methodMap of
          Just FuncInfo { funcId = f, .. } -> 
            throwPosError $ methodAlredyDeclaredError id clsId (position f)
            
          Nothing -> do
            Some retType <- someType retT
            paramTypes :&: _ <- getTypeList typeOfParam params
            let info = FuncInfo (debloat id) retType paramTypes
            return $ M.insert (name id) info methodMap
      

-- Utils ----------------------------------------------------------------------
typeOfParam :: S.Param -> S.Type
typeOfParam (S.Param t _) = t

getTypeList :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsType t
  ) =>
  (a -> t) -> [a] -> m (Sigma [LatteType] (TyCon1 (DList SLatteType)))
getTypeList _ [] = return $ SNil :&: DNil
getTypeList f (p : ps) = do
  Some tt <- someType $ f p
  tts :&: l <- getTypeList f ps
  return $ SCons tt tts :&: (tt :> l)




