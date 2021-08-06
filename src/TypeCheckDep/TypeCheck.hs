{-# LANGUAGE 
    MultiParamTypeClasses
  , FlexibleContexts
  , FlexibleInstances
  , RecordWildCards
  , GADTs
#-}

module TypeCheckDep.TypeCheck where

import qualified Data.Map as M
import Control.Monad.State hiding ( void )
import Control.Monad.Except hiding ( void )
import Data.Maybe

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error )

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import TypeCheckDep.State
import TypeCheckDep.StateUtils
import TypeCheckDep.LatteGetters
import TypeCheckDep.Declarations
import Errors ( Error )
import GenErrors
import Syntax.DebloaterDep ( ToBeDebloated(debloat), bloatId )
import Syntax.Bloater
import Syntax.BloaterDep
import Position.Position
import LangElemClasses hiding ( isVoid )
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
      
        S.FnDef _ retType id params _ -> do
          Some retT <- someType retType
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
    = updatePosTemp p $ do
        
        Some retT <- someType retType
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
        declParam acc (S.Param t id) = do
          acc
          Some tt <- someType t
          declareId tt id

        
  typeCheck (S.ClassDef p _ _ _)
    = updatePosTemp p $ throwPError internalClassToFuncDefError

          

instance ToBeTypeChecked S.TopDef ClassDef where
  --typeCheck (S.ClassDef p id maybeParent (S.ClassBody pp memberDecls))
  --  = throwTODO
  --{-
  typeCheck (S.ClassDef p id maybeParent (S.ClassBody pp memberDecls)) = do
    info@ClassInfo { classId = clsId, .. } <- getClassInfo id

    putSlefType $ clsId :&: extractParam2 (SCustom clsId)
    subVarScope >> subFuncScope
    depth <- declareMembers (Just info)
    
    okMemberDecls <- foldl appendTypeCheckedMember (pure []) memberDecls
    
    replicateM_ (depth + 1) (dropVarScope >> dropFuncScope)
    dropSlefType

    okMaybeParent <- getMbParent maybeParent
    let okBody = ClassBody pp okMemberDecls
    return $ ClassDef p (debloat id) okMaybeParent okBody

    where

      getMbParent  :: (MonadState TypeCheckState m, MonadError Error m)
        => Maybe S.Ident -> m (Maybe SomeClassIdent)
      getMbParent Nothing = return Nothing
      getMbParent (Just cls) = do
        ClassInfo { classId = clsId, .. } <- getClassInfo cls
        okCls <- getClassIdent (position cls) clsId
        return $ Just $ clsId :&: okCls


      declareMembers :: (MonadState TypeCheckState m, MonadError Error m)
        => Maybe ClassInfo -> m Int
      declareMembers Nothing = return 0
      declareMembers (Just (ClassInfo clsId parentInfo attrs methods pos))
        = do
          depth <- declareMembers parentInfo

          subVarScope >> subFuncScope
          foldl declAttr (pure ()) $ M.toList attrs
          foldl declMethod (pure ()) $ M.toList methods
          return $ depth + 1
          
          where
            declAttr :: (MonadState TypeCheckState m, MonadError Error m)
              => m () -> (String, VarInfo) -> m ()
            declAttr acc (_, VarInfo id t)
              = acc >> declareId t (bloatId id)

            declMethod :: (MonadState TypeCheckState m, MonadError Error m)
              => m () -> (String, FuncInfo) -> m ()
            declMethod acc (_, FuncInfo id retType argTypes _) = do
              acc
              cls <- getClassIdent fakePos clsId
              declareMethod cls (bloat id) retType argTypes

      appendTypeCheckedMember :: 
        (MonadState TypeCheckState m, MonadError Error m)
        => m [MemberDecl] -> S.MemberDecl -> m [MemberDecl]
      appendTypeCheckedMember = appendProcessed typeCheckMember

      typeCheckMember :: (MonadState TypeCheckState m, MonadError Error m)
        => S.MemberDecl -> m MemberDecl
      typeCheckMember decl = case decl of
        S.MethodDecl (S.ClassDef p _ _ _) -> throwError $ nestedClassError p
        
        S.AttrDecl p t id -> do
          Some tt <- someType t
          kwT <- getTypeKW (position t) tt
          return $ AttrDecl p kwT (debloat id)

        S.MethodDecl fnDef -> do
          okFnDef <- typeCheck fnDef
          return $ MethodDecl okFnDef
      -- -}
        
  typeCheck (S.FnDef p _ _ _ _)
    = updatePosTemp p $ throwPError internalFuncToClassDefError


instance ToBeTypeChecked S.Block Block where
  typeCheck (S.Block p stmts) = do
    subVarScope
    okStmts <- foldl appendTypeChecked (pure []) stmts
    dropVarScope
    return $ Block p okStmts

instance ToBeTypeChecked S.Stmt Stmt where
  typeCheck stmt = updatePosTemp stmt $ case stmt of
    S.Empty p -> return $ Empty p
    
    S.BStmt p block -> do
      okBlock <- typeCheck block
      return $ BStmt p okBlock

    S.Decl p t items -> do
      Some tt <- someType t
      kwT <- getTypeKW (position t) tt
      okItems <- foldl (declItem tt) (pure []) items
      return $ Decl p kwT okItems

      where
        declItem :: (MonadState TypeCheckState m, MonadError Error m)
          => SLatteType a -> m [Item a] -> S.Item -> m [Item a]
        declItem tt acc (S.NoInit id) = do
          l <- acc          
          declareId tt id
          return $ l ++ [NoInit (debloat id)]

        declItem tt acc (S.Init id expr) = do
            l <- acc
            declareId tt id
            okExpr <- getExpr tt expr
            return $ l ++ [Init (debloat id) okExpr]
          


    S.Ass p var expr -> do
      varType :&: okVar <- getAnyVar var
      okExpr <- getExpr varType expr
      return $ Ass p okVar okExpr

    S.Incr p var -> do
      okVar <- getVar STInt var
      return $ Incr p okVar

    S.Decr p var -> do
      okVar <- getVar STInt var
      return $ Decr p okVar
      
    S.Ret p expr -> do
      assertRetTypeIsSomething

      Some retType <- gets $ fromJust . returnType

      okExpr <- getExpr retType expr
      return $ Ret p okExpr

    S.VRet p -> do
      assertRetTypeIsSomething

      Some retType <- gets $ fromJust . returnType
      unless (isVoid retType) $ throwTPError returnVoidError retType
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
      _ :&: okExpr <- getAnyExpr expr
      return $ SExp p okExpr

    S.For p t id arr loopBody -> do
      Some tt <- someType t

      subVarScope

      declareId tt id
      okArr <- getVar (SArr tt) arr
      okLoopBody <- typeCheck loopBody

      dropVarScope
    
      kwT <- getTypeKW (position t) tt
      return $ For p kwT (debloat id) okArr okLoopBody

