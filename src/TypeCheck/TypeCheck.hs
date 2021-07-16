{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}

module TypeCheck.TypeCheck where

import Control.Monad.State hiding ( void )
import Control.Monad.Except hiding ( void )

import qualified Syntax.Syntax as S
import Syntax.SyntaxGADT
import TypeCheck.State
import TypeCheck.Getters
import TypeCheck.PuttersAndDeclarations
import Errors
import Syntax.Debloater ( ToBeDebloated(debloat) )
import Position.Position



class ToBeTypeChecked pre post where
  typeCheck :: (MonadState TypeCheckState m, MonadError Error m)
    => pre -> m post



appendTypeChecked ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    ToBeTypeChecked a b
  )
  => m [b] -> a -> m [b]
appendTypeChecked acc x = do
  l <- acc
  okX <- typeCheck x
  return $ l ++ [okX]


instance ToBeTypeChecked S.Program Program where
  typeCheck (S.Program p defs) = do
    foldl declare (pure ()) defs
    okDefs <- foldl appendTypeChecked (pure []) defs

    return $ Program p okDefs

    where
      
      declare :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.TopDef -> m ()
      declare _ def = case def of
        S.FnDef _ retType id params _ -> declareFunc id retType argTypes
          where
            argTypes = map typeOfParam params
        
        S.ClassDef _ id maybeParent body -> declareClass id maybeParent body




instance ToBeTypeChecked S.TopDef TopDef where
  typeCheck (S.FnDef p retType id params (S.Block blPos stmts))
    = case anyType retType of
        AnyT retT -> do
          subVarScope
          foldl declParam (pure ()) params

          subVarScope
          okStmts <- foldl appendTypeChecked' (pure []) stmts

          dropVarScope
          dropVarScope

          let okParams = map debloat params
          let okBlock = Block blPos okStmts
          let okDef = FnDef p retT (debloat id) okParams okBlock
          return okDef

    where
      declParam :: (MonadState TypeCheckState m, MonadError Error m)
        => m () -> S.Param -> m ()
      declParam _ (S.Param t id) = declareId t id
        


      appendTypeChecked' :: (MonadState TypeCheckState m, MonadError Error m)
        => m [Stmt] -> S.Stmt -> m [Stmt]
      appendTypeChecked' acc stmt = do
        l <- acc
        okStmt <- typeCheck' stmt
        return $ l ++ [okStmt]


      typeCheck' :: (MonadState TypeCheckState m, MonadError Error m)
        => S.Stmt -> m Stmt
      typeCheck' stmt = case anyType retType of
        AnyT retT -> case stmt of

          S.Ret retPos expr -> do
            okExpr <- getExpr retT expr
            return $ Ret retPos okExpr
          
          S.VRet retPos -> case retT of
            Void _ -> return $ VRet retPos 
            _ -> throwError $ returnVoidError retPos id retT
            
          
          _ -> typeCheck stmt


          


  typeCheck (S.ClassDef p id maybeParent body) = do
    throwTODO

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

    S.Decl p t items -> case anyType t of
      AnyT tt -> do
        okItems <- foldl (declItem t tt) (pure []) items
        return $ Decl p tt okItems

      where
        declItem :: (MonadState TypeCheckState m, MonadError Error m)
          => S.Type -> Type a -> m [Item a] -> S.Item -> m [Item a]
        declItem t tt acc (S.NoInit id) = do
          l <- acc          
          declareId t id
          return $ l ++ [NoInit (debloat id)]

        declItem t tt acc (S.Init id expr) = do
            l <- acc
            declareId t id
            okExpr <- getExpr tt expr
            return $ l ++ [Init (debloat id) okExpr]
          


    S.Ass p var expr -> do
      Any varType okVar <- getAnyVar var
      okExpr <- getExpr varType expr
      return $ Ass p okVar okExpr

    S.Incr p var -> do
      okVar <- getVar int var
      return $ Incr p okVar

    S.Decr p var -> do
      okVar <- getVar int var
      return $ Decr p okVar
      
    S.Ret p expr -> do
      Any _ okExpr <- getAnyExpr expr
      return $ Ret p okExpr

    S.VRet p -> return $ VRet p

    S.Cond p cond stm -> do
      okCond <- getExpr bool cond
      okStm <- typeCheck stm
      return $ Cond p okCond okStm

    S.CondElse p cond stmIf stmElse -> do
      okCond <- getExpr bool cond
      okStmIf <- typeCheck stmIf
      okStmElse <- typeCheck stmElse
      return $ CondElse p okCond okStmIf okStmElse

    S.While p cond loopBody -> do
      okCond <- getExpr bool cond
      okLoopBody <- typeCheck loopBody
      return $ While p okCond okLoopBody

    S.SExp p expr -> do
      Any _ okExpr <- getAnyExpr expr
      return $ SExp p okExpr

    S.For p t id arr loopBody -> case anyType t of
      AnyT tt -> do

        subVarScope

        declareId t id
        okArr <- getVar (Arr tt) arr
        okLoopBody <- typeCheck loopBody
      
        return $ For p tt (debloat id) okArr okLoopBody

