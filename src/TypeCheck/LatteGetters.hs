{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
#-}

module TypeCheck.LatteGetters where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Data.Singletons.Prelude hiding ( Error, Any )
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

import Dependent


-------------------------------------------------------------------------------
getCallableInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m CallableInfo
getCallableInfo var = case var of

  S.Var p id -> do
    FuncInfo funcId retT paramTs pp <- getFuncInfo id
    
    return $ CallableInfo (Func p funcId) retT paramTs pp

  S.Member p e id -> do
    
    ownerType :&: owner <- getAnyExpr e
    
    case ownerType of

      SArr _ -> throwError $ noArrMethodError memberPos id
      
      SCustom cls -> do
        info <- getClassInfo $ mkClsId (position owner) cls
        let err = noClsMethodError memberPos ownerType id
        FuncInfo methId retT paramTs pp <- getMethodInfo err info id

        return $ CallableInfo (Method p owner methId) retT paramTs pp
        
      
      _ -> throwError $ noMethodError memberPos ownerType id
    
      where memberPos = position id

  S.Elem p e1 e2 -> throwError $ notAFuncError p var

  S.Null p -> throwError $ notAFuncError p var

  S.Self p -> throwError $ notAFuncError p var

  where

  getMethodInfo :: MonadError Error m
    => Error -> ClassInfo -> S.Ident -> m FuncInfo
  getMethodInfo err info id = case M.lookup (name id) (methods info) of
    Just methodInfo -> return methodInfo
    Nothing -> case parent info of
      Nothing         -> throwError err
      Just parentInfo -> getMethodInfo err parentInfo id




-------------------------------------------------------------------------------
getAnyVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Any Var)
getAnyVar var = case var of
  S.Var p id -> do
    VarInfo x t _ <- getIdentInfo id
    
    return $ t :&: Var p x

  S.Member p e id -> do
    
    ownerType :&: owner <- getAnyExpr e

    case ownerType of
      SArr t -> do
        unless (name id == lengthAttr)
          $ throwError $ noArrAttrError memberPos id
        return $ STInt :&: Attr p owner (debloat id)

      SCustom cls -> do
        info <- getClassInfo $ mkClsId (position owner) cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwError $ noAttributeError memberPos ownerType id
          Just (VarInfo _ t _) -> return $ t :&: Attr p owner (debloat id)
      
      _ -> throwError $ noAttributeError memberPos ownerType id

      where memberPos = position id

  S.Elem p e1 e2 -> do
    arrType :&: arr <- getAnyExpr e1

    case arrType of
      SArr t -> do
        i <- getExpr STInt e2
        return $ t :&: Elem p arr i

      _ -> throwError $ notAnArrayArror (position e2) e1
  
  S.Null p -> return $ STNull :&: Null p

  S.Self p -> do
    _ :&: selfT <- getSelfType p
    return $ insertParam2 selfT :&: Self p

getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.Var -> m (Var a)

getVar t var = do
  tt :&: v <- getAnyVar var
  
  let err = wrongVarTypeError (position var) var t tt
  filterT (position var) err t tt v

getTypeOfVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Some SLatteType)
getTypeOfVar var = do
  tt :&: v <- getAnyVar var
  return $ Some tt



-------------------------------------------------------------------------------
getAnyExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Any Expr)
getAnyExpr expr = case expr of

  ---------------------------------------------------------------------
  S.EVar p v -> do
    t :&: var <- getAnyVar v
    return $ t :&: EVar p t var
    
  S.ELitInt   p (S.SInt _ i)  -> return $ STInt   :&: ELitInt   p i
  S.ELitBool  p b             -> return $ STBool  :&: ELitBool  p b
  S.EString   p (S.SStr _ s)  -> return $ STStr   :&: EString   p s

  ---------------------------------------------------------------------
  S.Neg p e -> do
    okExpr <- getExpr STInt e
    return $ STInt :&: Neg p okExpr

  S.Not p e -> do
    okExpr <- getExpr STBool e
    return $ STBool :&: Not p okExpr

  ---------------------------------------------------------------------
  S.EOp p op lhs rhs -> case op of
    S.Plus _ -> do
      Some operandType <- getTypeOfExpr lhs
      case operandType of
        STStr -> getStrOpExpr p lhs rhs
        STInt -> getIntOpExpr p op lhs rhs
        _     -> throwError
                  $ exprTypeNotInListError p expr expectedList operandType

          where expectedList = [Some STInt, Some STStr]

    _ -> getIntOpExpr p op lhs rhs

    where
    
      getIntOpExpr :: (MonadState TypeCheckState m, MonadError Error m)
        => Pos -> S.BinOp -> S.Expr -> S.Expr -> m (Any Expr)
      getIntOpExpr p op lhs rhs = do
        okLHS <- getExpr STInt lhs
        okRHS <- getExpr STInt rhs
        return $ STInt :&: EOp p (debloat op) okLHS okRHS
      
      getStrOpExpr :: (MonadState TypeCheckState m, MonadError Error m)
        => Pos -> S.Expr -> S.Expr -> m (Any Expr)
      getStrOpExpr p lhs rhs = do
        okLHS <- getExpr STStr lhs
        okRHS <- getExpr STStr rhs
        return $ STStr :&: Concat p okLHS okRHS


  S.ERel p op lhs rhs -> do

    Some lhsType <- getTypeOfExpr lhs
    Some rhsType <- getTypeOfExpr rhs
    
    let err = wrongExprTypeError p rhs lhsType rhsType
    let errCls = typesNotCompatibileError p lhs rhs lhsType rhsType
    
    Some t <- getCommonType (position expr) err errCls lhsType rhsType
    
    okOp <- getOp t op
    
    okLHS <- getExpr t lhs
    okRHS <- getExpr t rhs
    
    return $ STBool :&: ERel p t okOp okLHS okRHS



  S.EBool p op lhs rhs -> do
    okLHS <- getExpr STBool lhs
    okRHS <- getExpr STBool rhs
    return $ STBool :&: EBool p (debloat op) okLHS okRHS

  ---------------------------------------------------------------------
  S.EApp p v args -> do
    CallableInfo okV retType paramTypes _ <- getCallableInfo v
    okArgs <- validateArgs p v args paramTypes
    return $ retType :&: EApp p okV okArgs
    
  ---------------------------------------------------------------------
  S.NewArr p elemType intExpr -> do
    i <- getExpr STInt intExpr

    elemT :&: elemTKW <- getSomeTypeKW elemType
    return $ SArr elemT :&: NewArr p elemTKW i
  

  S.NewObj p clsId -> do
    ClassInfo { classId = cls, className = clsN, .. } <- getClassInfo clsId
    return $ SCustom clsN :&: NewObj p (KWCustom cls)

  S.Cast p t e -> do
    eType :&: okExpr <- getAnyExpr e
    tt :&: kw <- getSomeTypeKW t
    return $ tt :&: Cast p kw okExpr
    

getExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.Expr -> m (Expr a)
getExpr t expr = do
  tt :&: okExpr <- getAnyExpr expr
  let err = wrongExprTypeError (position okExpr) expr t tt
  filterT (position expr) err t tt okExpr

getTypeOfExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Some SLatteType)
getTypeOfExpr expr = do
  t :&: okExpr <- getAnyExpr expr
  return $ Some t





getOp :: (MonadError Error m) => SLatteType a -> S.RelOp -> m (RelOp a)
getOp t op = case (op, t) of
  (S.LTH p, STInt)  -> return $ LTH p
  (S.LE  p, STInt)  -> return $ LE  p
  (S.GTH p, STInt)  -> return $ GTH p
  (S.GE  p, STInt)  -> return $ GE  p
  (S.EQU p, _)      -> return $ EQU p
  (S.NE  p, _)      -> return $ NE  p
  
  (r, t) -> throwError $ wrongOpTypeError (position op) r t


validateArgs :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Var -> [S.Expr] -> SList ts -> m (ExprList ts)
validateArgs p v [] SNil = return DNil

validateArgs p v (expr : exprs) (SCons t ts) = do
  okExpr <- getExpr t expr
  okExprs <- validateArgs p v exprs ts
  return $ okExpr :> okExprs
      
validateArgs p v args paramTypes
  = throwError $ wrongNOParamsError p v (sLengthInt paramTypes) (length args)


