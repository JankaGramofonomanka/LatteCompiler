{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
#-}

module TypeCheckDep.LatteGetters where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error, Any )

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors ( Error )
import GenErrors
import LangElemClasses
import Syntax.DebloaterDep
import qualified Scope as Sc
import TypeCheckDep.State
import TypeCheckDep.StateUtils
import Dependent


-- Functions ------------------------------------------------------------------
type SomeCollable = Sigma2 LatteType [LatteType] (TyCon2 Callable)

-- {-
getCallableInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m CallableInfo
getCallableInfo var = updatePosTemp var $ case var of

  S.Var p id -> do
    FuncInfo funcId retType paramTypes <- getFuncInfo id
    
    return $ CallableInfo (Func p funcId) retType paramTypes

  S.Member p e id -> do
    
    ownerT :&: owner <- getAnyExpr e
    
    updatePosTemp id $ case ownerT of

      SArr _ -> throwPError $ noArrMethodError id
      
      SCustom clsId -> do

        cls <- getClassIdent p clsId
        info <- getClassInfo cls
        let err = noClsMethodError id
        FuncInfo methId retType paramTypes <- getMethodInfo err ownerT info id

        return $ CallableInfo (Method p owner methId) retType paramTypes
        
      
      _ -> throwTPError (noMethodError id) ownerT
    

  _ -> throwPError $ notAFuncError var

  where

    getMethodInfo :: (MonadState TypeCheckState m, MonadError Error m)
      => TPError t -> SLatteType t -> ClassInfo -> S.Ident -> m FuncInfo
    getMethodInfo err t info id = updatePosTemp id
      $ case M.lookup (name id) (methods info) of
        Just methodInfo -> return methodInfo
        Nothing -> case parent info of
          Nothing         -> throwTPError err t
          Just parentInfo -> getMethodInfo err t parentInfo id


-- Variables ------------------------------------------------------------------
getAnyVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Any Var)
getAnyVar var = updatePosTemp var $ case var of
  S.Var p id -> do
    VarInfo x t <- getIdentInfo id
    
    return $ t :&: Var p x

  S.Member p e id -> do
    
    ownerT :&: owner <- getAnyExpr e

    updatePosTemp id $ case ownerT of
      SArr t -> do
        unless (name id == lengthAttr) $ throwPError $ noArrAttrError id
        return $ STInt :&: Attr p owner (debloat id)

      
      SCustom clsId -> do

        cls <- getClassIdent p clsId
        info <- getClassInfo cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwTPError (noAttributeError id) ownerT
          Just (VarInfo _ t) -> return $ t :&: Attr p owner (debloat id)
      
      _ -> throwTPError (noAttributeError id) ownerT


  S.Elem p e1 e2 -> do
    arrType :&: arr <- getAnyExpr e1

    updatePosTemp e1 $ case arrType of
      SArr t -> do
        i <- getExpr STInt e2
        return $ t :&: Elem p arr i

      _ -> throwPError $ notAnArrayArror e1
  
  S.Null p -> return $ STNull :&: Null p

  S.Self p -> do
    n :&: selfT <- getSelfType
    return $ insertParam2 selfT :&: Self p

getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.Var -> m (Var a)

getVar t var = updatePosTemp var $ do
  tt :&: v <- getAnyVar var
  
  let err = wrongVarTypeError var
  filterT err t tt v

getTypeOfVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Some SLatteType)
getTypeOfVar var = updatePosTemp var $ do
  tt :&: v <- getAnyVar var
  return $ Some tt



-- Expressions ----------------------------------------------------------------
getAnyExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Any Expr)
getAnyExpr expr = updatePosTemp expr $ case expr of

  ---------------------------------------------------------------------
  S.EVar p v -> do
    t :&: var <- getAnyVar v
    return $ t :&: EVar p var
    
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
        _     -> 
          throwTPError (exprTypeNotInListError expr expectedList) operandType

          where expectedList = [Some $ KWInt fakePos, Some $ KWStr fakePos]

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
    
    --let err = wrongExprTypeError rhs
    --let errCls = typesNotCompatibileError lhs rhs
    
    
    -- TODO
    -- Some t <- getCommonType err errCls lhsType rhsType
    let t = lhsType
    
    okOp <- getOp t op
    
    okLHS <- getExpr t lhs
    okRHS <- getExpr t rhs
    
    return $ STBool :&: ERel p okOp okLHS okRHS



  S.EBool p op lhs rhs -> do
    okLHS <- getExpr STBool lhs
    okRHS <- getExpr STBool rhs
    return $ STBool :&: EBool p (debloat op) okLHS okRHS

  ---------------------------------------------------------------------
  S.EApp p v args -> do
    CallableInfo okV retType paramTypes <- getCallableInfo v
    okArgs <- validateArgs p v args paramTypes
    return $ retType :&: EApp p okV okArgs
    
  ---------------------------------------------------------------------
  S.NewArr p elemType intExpr -> do
    i <- getExpr STInt intExpr

    Some elemT <- someType elemType
    kwElemT <- getTypeKW (position elemType) elemT
    return $ SArr elemT :&: NewArr p kwElemT i
  

  S.NewObj p clsId -> do
    ClassInfo { classId = cls, .. } <- getClassInfo clsId
    clsId <- getClassIdent p cls
    return $ SCustom cls :&: NewObj p (KWCustom clsId)

  S.Cast p t e -> do
    eType :&: okExpr <- getAnyExpr e
    Some tt <- someType t
    kwTT <- getTypeKW (position t) tt
    return $ tt :&: Cast p kwTT okExpr
    

getExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.Expr -> m (Expr a)
getExpr t expr = updatePosTemp expr $ do
  tt :&: okExpr <- getAnyExpr expr
  
  let err = wrongExprTypeError expr
  filterT err t tt okExpr

getTypeOfExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Some SLatteType)
getTypeOfExpr expr = updatePosTemp expr $ do
  t :&: okExpr <- getAnyExpr expr
  return $ Some t





getOp :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.RelOp -> m (RelOp a)
getOp t op = updatePosTemp op $ case (op, t) of
  (S.LTH p, STInt)  -> return $ LTH p
  (S.LE  p, STInt)  -> return $ LE  p
  (S.GTH p, STInt)  -> return $ GTH p
  (S.GE  p, STInt)  -> return $ GE  p
  (S.EQU p, _)      -> return $ EQU p
  (S.NE  p, _)      -> return $ NE  p
  
  (r, t) -> throwTPError (wrongOpTypeError r) t



validateArgs :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Var -> [S.Expr] -> SList ts -> m (ExprList ts)
validateArgs p v [] SNil = return DNil

validateArgs p v (expr : exprs) (SCons t ts) = updatePosTemp expr $ do
  okExpr <- getExpr t expr
  okExprs <- validateArgs p v exprs ts
  return $ okExpr :> okExprs
      
validateArgs p v args paramTypes = updatePosTemp p
  $ throwPError $ wrongNOParamsError v (sLengthInt paramTypes) (length args)

