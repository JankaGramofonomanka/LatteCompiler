{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , DataKinds
  , KindSignatures
#-}

module TypeCheckDep.Declarations where

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
import LangElemClasses hiding ( isVoid )
import Syntax.DebloaterDep
import qualified Scope as Sc
import TypeCheckDep.State
import TypeCheckDep.StateUtils
import TypeCheckDep.LatteGetters
import Dependent



-------------------------------------------------------------------------------
declareId ::
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => SLatteType t -> S.Ident -> m ()
declareId t id = do
    
    when (isVoid t) $ throwPError $ voidDeclarationError id

    varScope <- gets varScope
    
    let varInfo = VarInfo (debloat id) t
    updatePosTemp id $ case Sc.insertNew (name id) varInfo varScope of
      Nothing -> do
        VarInfo { varId = declared, .. } <- getIdentInfo id
        let declaredAt = position declared
        throwPError $ varAlredyDeclaredError id declaredAt
      
      Just newScope -> putVarScope newScope


declareFunc ::
  ( MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareFunc = declareCallable (Nothing :: Maybe S.Ident)

declareMethod ::
  (MonadState TypeCheckState m, MonadError Error m, IsIdent i)
  => i -> S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareMethod i = declareCallable (Just i)


-- TODO how to update position
declareCallable ::
  (MonadState TypeCheckState m, MonadError Error m, IsIdent i )
  => Maybe i -> S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareCallable ownerCls funcId retT paramTypes = do
    
    fnScope <- gets funcScope
    let info = FuncInfo (debloat funcId) retT paramTypes

    updatePosTemp funcId $ case Sc.insertNew (name funcId) info fnScope of

      Nothing -> do
        
        FuncInfo { funcId = declared, .. } <- getFuncInfo funcId
        let declaredAt = position declared
        
        case ownerCls of
          Just cls -> throwPError
            $ methodAlredyDeclaredError funcId cls declaredAt
            
          Nothing -> throwPError
            $ funcAlredyDeclaredError funcId declaredAt
      
      Just newScope -> putFuncScope newScope



declareClass :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> Maybe S.Ident -> S.ClassBody -> m ()
declareClass id maybeParent body = do
  clsMap <- gets classMap

  updatePosTemp id $ case M.lookup (name id) clsMap of
    Just ClassInfo { classId = c, declaredAt = p, .. } -> throwPError
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
            updatePosTemp id $ throwPError $ noSuchClassError id

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
          throwPError $ attrAlredyDeclaredError id clsId (position x)
          
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
      = updatePosTemp p $ throwPError nestedClassError

    addMethod acc (S.MethodDecl (S.FnDef p retT id params body))
      = updatePosTemp p $ do
        methodMap <- acc

        case M.lookup (name id) methodMap of
          Just FuncInfo { funcId = f, .. } -> 
            throwPError $ methodAlredyDeclaredError id clsId (position f)
            
          Nothing -> do
            Some retType <- someType retT
            paramTypes :&: _ <- getParamList params
            let info = FuncInfo (debloat id) retType paramTypes
            return $ M.insert (name id) info methodMap

getParamList :: 
  ( MonadState TypeCheckState m,
    MonadError Error m
  ) =>
  [S.Param] -> m (Sigma [LatteType] (TyCon1 (DList Ident)))
getParamList [] = return $ SNil :&: DNil
getParamList ((S.Param t paramId) : ps) = do
  Some tt <- someType t
  tts :&: l <- getParamList ps
  return $ SCons tt tts :&: (debloat paramId :> l)

  

typeOfParam :: S.Param -> S.Type
typeOfParam (S.Param t _) = t



