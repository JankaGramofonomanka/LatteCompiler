{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , DataKinds
  , KindSignatures
  , ScopedTypeVariables
#-}

module TypeCheck.Declarations where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Prelude hiding ( Error )
import Data.Singletons.Sigma

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors
import LangElemClasses
import Syntax.Debloater
import qualified Scope as Sc
import TypeCheck.State
import TypeCheck.StateUtils
import TypeCheck.LatteGetters

import Dependent
import SingChar


-------------------------------------------------------------------------------
declareId ::
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => Pos -> SLatteType t -> S.Ident -> m ()
declareId declarationPos t id = case someType t of
  Some tt -> do
    
    when (isVoid t) $ throwError $ voidDeclarationError declarationPos id

    varScope <- gets varScope
    
    let varInfo = VarInfo (debloat id) tt (position id)
    case Sc.insertNew (name id) varInfo varScope of
      Nothing -> do
        VarInfo { varId = declared, .. } <- getIdentInfo id
        let declaredAt = position declared
        throwError $ varAlredyDeclaredError (position id) id declaredAt
      
      Just newScope -> putVarScope newScope


declareFunc :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareFunc = declareCallable (Nothing :: Maybe S.Ident)

declareMethod :: (MonadState TypeCheckState m, MonadError Error m, IsIdent i)
  => i -> S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareMethod i = declareCallable (Just i)
    
declareCallable :: (MonadState TypeCheckState m, MonadError Error m, IsIdent i)
  => Maybe i -> S.Ident -> SLatteType t -> SList (ts :: [LatteType]) -> m ()
declareCallable ownerCls id retType paramTypes = case someType retType of
  Some retT -> do
    
    fnScope <- gets funcScope
    let info = FuncInfo (debloat id) retT paramTypes (position id)
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
    
    Nothing -> case someFromString (name id) of
      SomeSing clsName -> do
        parentInfo <- getParentInfo maybeParent
        attrMap <- getAttrMap id body
        methodMap <- getMethodMap id body

        let clsId = debloat id
        let clsP = position id
        let clsInfo = ClassInfo clsId clsName parentInfo attrMap methodMap clsP
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
          
        Nothing -> case someType t of
          Some tt -> do
            let info = VarInfo (debloat id) tt p
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
      = throwError $ nestedClassError p

    addMethod acc (S.MethodDecl (S.FnDef p retT id params body)) = do
      methodMap <- acc

      case M.lookup (name id) methodMap of
        Just FuncInfo { funcId = f, .. } -> 
          throwError $ methodAlredyDeclaredError p id clsId (position f)
          
        Nothing -> case someType retT of
          Some retType -> do
            paramTypes :&: _ <- getParamList params
            let info = FuncInfo (debloat id) retType paramTypes (position p)
            return $ M.insert (name id) info methodMap

getParamList :: 
  ( MonadState TypeCheckState m,
    MonadError Error m
  ) =>
  [S.Param] -> m (Sigma [LatteType] (TyCon1 (DList Param)))
getParamList [] = return $ SNil :&: DNil
getParamList (p@(S.Param t paramId) : ps) = case someType t of
  Some tt -> do
    kwT <- getTypeKW (position t) tt
    tts :&: l <- getParamList ps
    return $ SCons tt tts :&: (Param kwT (debloat paramId) :> l)


typeOfParam :: S.Param -> S.Type
typeOfParam (S.Param t _) = t

