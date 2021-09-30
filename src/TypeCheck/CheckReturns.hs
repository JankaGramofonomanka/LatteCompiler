{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GADTs
#-}
module TypeCheck.CheckReturns where

import Control.Monad.Except

import Errors
import Position.Position
import Position.EndPosition
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS

class HasReturns a where
  assertReturns :: MonadError Error m => a -> m ()
  checkReturns :: MonadError Error m => a -> m a
  checkReturns x = assertReturns x >> return x

class HasReturnsInFunc a where
  assertReturnsInFunc :: MonadError Error m
    => Pos -> DS.FuncIdent t ts -> a -> m ()
  
  assertOneReturn :: MonadError Error m
    => Pos -> DS.FuncIdent t ts -> a -> m ()

ok :: Monad m => m ()
ok = return ()

instance HasReturnsInFunc DS.Stmt where
  assertReturnsInFunc p f stmt = case stmt of
    
    DS.Ret       {} -> ok
    DS.VRet      {} -> ok

    DS.BStmt _ block -> assertReturnsInFunc p f block
    
    DS.CondElse _ _ stm1 stm2 ->
      assertReturnsInFunc p f stm1 >> assertReturnsInFunc p f stm2

    DS.While _ _ stm          -> assertOneReturn p f stm
    DS.For _ _ _ _ stm        -> assertOneReturn p f stm
    DS.Forever _ stm          -> assertOneReturn p f stm
    
    _ -> throwError $ missingReturnError p f

  assertOneReturn p f stmt = case stmt of
    DS.Cond _ _ stm ->
      assertOneReturn p f stm

    DS.CondElse _ _ stm1 stm2 ->
      catchError (assertOneReturn p f stm1) (\_ -> assertOneReturn p f stm2)
    
    DS.BStmt _ block -> assertOneReturn p f block

    _ -> assertReturnsInFunc p f stmt
      



instance HasReturnsInFunc [DS.Stmt] where
  assertReturnsInFunc p f stmts = case stmts of
          
    []  -> throwError $ missingReturnError p f
    _   -> assertReturnsInFunc p f (last stmts)

  assertOneReturn p f (stm : stmts) = do
    catchError (assertOneReturn p f stm) (\_ -> assertOneReturn p f stmts)
  
  assertOneReturn p f [] = throwError $ missingReturnError p f

instance HasReturnsInFunc DS.Block where
  assertReturnsInFunc p f (DS.Block _ _ stmts) = assertReturnsInFunc p f stmts
  assertOneReturn p f (DS.Block _ _ stmts) = assertOneReturn p f stmts







instance HasReturns DS.FnDef where
  assertReturns def@(DS.FnDef p t f _ body)
    = case DS.singFromKW t of
      DS.STVoid -> ok
      _         -> assertReturnsInFunc (endPosition def) f body

instance HasReturns DS.MemberDecl where
  assertReturns decl@DS.AttrDecl {} = ok
  assertReturns decl@(DS.MethodDecl f) = assertReturns f

instance HasReturns DS.ClassBody where
  assertReturns body@(DS.ClassBody _ _ memberDecls)
    = mapM_ assertReturns memberDecls

instance HasReturns DS.ClassDef where
  assertReturns def@(DS.ClassDef p clsId mbParent body) = assertReturns body

instance HasReturns DS.Program where
  assertReturns prog@(DS.Program _ defs) = mapM_ assertReturns' defs

    where

      assertReturns' (Left def) = Left <$> assertReturns def
      assertReturns' (Right def) = Right <$> assertReturns def






