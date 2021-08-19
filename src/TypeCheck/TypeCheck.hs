{-# LANGUAGE 
    MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  , RecordWildCards
  , GADTs
#-}

module TypeCheck.TypeCheck where

import qualified Data.Map as M
import Control.Monad.State hiding ( void )
import Control.Monad.Except hiding ( void )
import Data.Maybe

import Data.Singletons.Prelude hiding ( Error )
import Data.Singletons.Sigma

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import TypeCheck.State
import TypeCheck.StateUtils
import TypeCheck.LatteGetters
import TypeCheck.Declarations
import Errors
import Syntax.Debloater ( ToBeDebloated(debloat), bloatId, debloatScopedId )
import Syntax.Bloater
import Position.Position
import LangElemClasses

import Dependent


class ToBeTypeChecked pre post where
  typeCheck :: (MonadState TypeCheckState m, MonadError Error m)
    => pre -> m post


appendProcessed :: Monad m => (a -> m b) -> m [b] -> a -> m [b]
appendProcessed process acc x = do
  l <- acc
  okX <- process x
  return $ l ++ [okX]


appendTypeChecked ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    ToBeTypeChecked a b
  )
  => m [b] -> a -> m [b]
appendTypeChecked = appendProcessed typeCheck

instance ToBeTypeChecked S.Program Program where
  typeCheck (S.Program p defs) = do
    foldl declare (pure ()) defs
    okDefs <- foldl appendTypeChecked (pure []) defs

    return $ Program p okDefs

    where
      
      declare :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.TopDef -> m ()
      declare acc def = acc >> case def of
        S.FnDef _ retType id params _ -> case someType retType of
          Some retT -> do
            paramTypes :&: _ <- getParamList params
            declareFunc id retT paramTypes
        
        S.ClassDef _ id maybeParent body -> declareClass id maybeParent body




instance ToBeTypeChecked S.TopDef (Either ClassDef FnDef) where
  typeCheck def@S.FnDef {} = do
    okDef <- typeCheck def
    return $ Right okDef

  typeCheck def@S.ClassDef {} = do
    okDef <- typeCheck def
    return $ Left okDef
  



instance ToBeTypeChecked S.TopDef FnDef where
  typeCheck (S.FnDef p retType id params (S.Block blPos stmts))
    = case someType retType of
        Some retT -> do
          subVarScope
          foldl declParam (pure ()) params

          subVarScope
          putReturnType retType
          okStmts <- foldl appendTypeChecked (pure []) stmts
          dropReturnType

          dropVarScope
          dropVarScope

          _ :&: okParams <- getParamList params
          let okBlock = Block blPos okStmts
          retTKW <- getTypeKW (position retType) retT
          let okDef = FnDef p retTKW (debloat id) okParams okBlock
          return okDef

    where
      declParam :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.Param -> m ()
      declParam acc (S.Param t id) = case someType t of
        Some tt -> acc >> declareId (position t) 0 tt id
        

  typeCheck (S.ClassDef p _ _ _)
    = throwError $ internalClassToFuncDefError p

          
          

instance ToBeTypeChecked S.TopDef ClassDef where
  typeCheck (S.ClassDef p id maybeParent (S.ClassBody pp memberDecls)) = do
    info@ClassInfo { classId = clsId, className = clsN, .. } <- getClassInfo id

    putSlefType $ SCustom clsN
    subVarScope >> subFuncScope
    depth <- declareMembers (Just info)
    
    okMemberDecls <- foldl appendTypeCheckedMember (pure []) memberDecls
    
    replicateM_ (depth + 1) (dropVarScope >> dropFuncScope)
    dropSlefType

    okMaybeParent <- getMbParent maybeParent
    let okBody = ClassBody pp okMemberDecls
    return $ ClassDef p clsId okMaybeParent okBody

    where

      getMbParent  :: (MonadState TypeCheckState m, MonadError Error m)
        => Maybe S.Ident -> m (Maybe SomeClassIdent)
      getMbParent Nothing = return Nothing
      getMbParent (Just cls) = do
        ClassInfo { classId = clsId, className = clsN, .. } <- getClassInfo cls
        return $ Just $ clsN :&: clsId

      declareMembers :: (MonadState TypeCheckState m, MonadError Error m)
        => Maybe ClassInfo -> m Int
      declareMembers Nothing = return 0
      declareMembers (Just (ClassInfo clsId _ parentInfo attrs methods _))
        = do
          depth <- declareMembers parentInfo

          subVarScope >> subFuncScope
          foldl declAttr (pure ()) $ M.toList attrs
          foldl declMethod (pure ()) $ M.toList methods
          return $ depth + 1
          
          where
            declAttr :: (MonadState TypeCheckState m, MonadError Error m)
              => m () -> (String, VarInfo) -> m ()
            declAttr acc (_, VarInfo id t p)
              = acc >> declareId p 0 t (bloat id)

            declMethod :: (MonadState TypeCheckState m, MonadError Error m)
              => m () -> (String, FuncInfo) -> m ()
            declMethod acc (_, FuncInfo id retType argTypes _) = do
              acc
              declareMethod clsId (bloat id) retType argTypes

      appendTypeCheckedMember :: 
        (MonadState TypeCheckState m, MonadError Error m)
        => m [MemberDecl] -> S.MemberDecl -> m [MemberDecl]
      appendTypeCheckedMember = appendProcessed typeCheckMember

      typeCheckMember :: (MonadState TypeCheckState m, MonadError Error m)
        => S.MemberDecl -> m MemberDecl
      typeCheckMember decl = case decl of
        S.MethodDecl (S.ClassDef p _ _ _) -> throwError $ nestedClassError p
        
        S.AttrDecl p t id -> do
            _ :&: kwT <- getSomeTypeKW t
            return $ AttrDecl p kwT (debloat id)

        S.MethodDecl fnDef -> do
          okFnDef <- typeCheck fnDef
          return $ MethodDecl okFnDef
        
  typeCheck (S.FnDef p _ _ _ _)
    = throwError $ internalFuncToClassDefError p




instance ToBeTypeChecked S.Block Block where
  typeCheck (S.Block p stmts) = do
    subVarScope
    okStmts <- foldl appendTypeChecked (pure []) stmts
    dropVarScope
    return $ Block p okStmts

instance ToBeTypeChecked S.Stmt Stmt where
  typeCheck stmt = case stmt of
    S.Empty p -> return $ Empty p
    
    S.BStmt p block -> do
      okBlock <- typeCheck block
      return $ BStmt p okBlock

    S.Decl p t items -> do
        tt :&: kwT <- getSomeTypeKW t
        okItems <- foldl (declItem tt) (pure []) items        
        return $ Decl p kwT okItems

      where
        declItem :: (MonadState TypeCheckState m, MonadError Error m)
          => SLatteType a -> m [Item a] -> S.Item -> m [Item a]
        declItem tt acc (S.NoInit id) = do
          l <- acc 
          lvl <- gets currentScopeLevel
          declareId (position t) lvl tt id
          return $ l ++ [NoInit (debloatScopedId lvl id)]

        declItem tt acc (S.Init id expr) = do
            l <- acc
            lvl <- gets currentScopeLevel
            okExpr <- getExpr tt expr
            declareId (position t) lvl tt id
            return $ l ++ [Init (debloatScopedId lvl id) okExpr]
          


    S.Ass p var expr -> do
      varType :&: okVar <- getAnyVar var
      okExpr <- getExpr varType expr
      return $ Ass p varType okVar okExpr

    S.Incr p var -> do
      okVar <- getVar STInt var
      return $ Incr p okVar

    S.Decr p var -> do
      okVar <- getVar STInt var
      return $ Decr p okVar
      
    S.Ret p expr -> do
      assertRetTypeIsSomething p

      Some retType <- gets $ fromJust . returnType

      okExpr <- getExpr retType expr
      return $ Ret p retType okExpr

    S.VRet p -> do
      assertRetTypeIsSomething p

      Some retType <- gets $ fromJust . returnType
      unless (isVoid retType) $ throwError $ returnVoidError p retType
      return $ VRet p

    S.Cond p cond stm -> do
      okCond <- getExpr STBool cond
      okStm <- typeCheck stm
      return $ Cond p okCond okStm

    S.CondElse p cond stmIf stmElse -> do
      okCond <- getExpr STBool cond
      okStmIf <- typeCheck stmIf
      okStmElse <- typeCheck stmElse
      return $ CondElse p okCond okStmIf okStmElse

    S.While p cond loopBody -> do
      okCond <- getExpr STBool cond
      okLoopBody <- typeCheck loopBody
      return $ While p okCond okLoopBody

    S.SExp p expr -> do
      t :&: okExpr <- getAnyExpr expr
      return $ SExp p t okExpr

    S.For p t id arr loopBody -> do
        tt :&: kwT <- getSomeTypeKW t

        subVarScope

        lvl <- gets currentScopeLevel
        declareId p lvl tt id
        okArr <- getVar (SArr tt) arr
        okLoopBody <- typeCheck loopBody

        dropVarScope
      
        return $ For p kwT (debloat id) okArr okLoopBody

