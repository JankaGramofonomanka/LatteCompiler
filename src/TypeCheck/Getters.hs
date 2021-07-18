{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}

module TypeCheck.Getters where

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


newtype X1 (a :: * -> *) b = X1 (a b)
newtype X2 (a :: * -> *) (b :: * -> *) c = X2 (a (b c))
toX2 :: e (a b) -> X2 e a b
toX2 = X2

fromX2 :: X2 e a b -> e (a b)
fromX2 (X2 x) = x

filterT :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> Type a -> Type b -> e b -> m (e a)
filterT _ (Int _)  (Int _)  x = return x
filterT _ (Str _)  (Str _)  x = return x
filterT _ (Bool _) (Bool _) x = return x
filterT err (Arr t1) (Arr t2) x = do
  xx <- filterT err t1 t2 (toX2 x)
  
  return $ fromX2 xx

filterT err (Custom id1) (Custom id2) x = do
  
    assertSubClass err id1 id2
    return x

filterT err expected actual x = throwError err

assertSubClass :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> Ident Class -> Ident Class -> m ()
assertSubClass err parent child = do
  if name parent == name child then
    return ()
  else do
    ClassInfo _ mbParent _ _ <- getClassInfo child
    case mbParent of
      Nothing -> throwError err
      Just (ClassInfo parentId _ _ _) -> assertSubClass err parent parentId 

getIdentInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m VarInfo
getIdentInfo id = do
  varScope <- gets varScope
  case Sc.lookup (name id) varScope of
    Nothing   -> throwError $ noSuchVarError (position id) id
    Just info -> return info

getIdent :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => Type a -> i -> m (Ident a)
getIdent expT id = do
  (VarInfo x actT) <- getIdentInfo id
  let err = wrongIdentTypeError (position id) id expT actT 
  filterT err expT actT x


getFuncInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncInfo
getFuncInfo id = do
  fnScope <- gets funcScope
  case Sc.lookup (name id) fnScope of
    Nothing   -> throwError $ noSuchFuncError (position id) id
    Just info -> return info
    
getClassInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m ClassInfo
getClassInfo id = do
  clsMap <- gets classMap
  case M.lookup (name id) clsMap of
    Nothing   -> throwError $ noSuchClassError (position id) id
    Just info -> return info

getFunc ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncIdent
getFunc id = do
  info <- getFuncInfo id
  return $ funcId info
    
getClass ::
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m ClassIdent
getClass id = do
  info <- getClassInfo id
  return $ classId info


-------------------------------------------------------------------------------
getCallableVarAndInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Var Func, FuncInfo)
getCallableVarAndInfo var = case var of

  S.Var p id -> do
    info <- getFuncInfo id
    
    return (Var p (funcId info), info)

  S.Member p v id -> do
    
    Any ownerType owner <- getAnyVar v
    
    case ownerType of

      Arr _ -> throwError $ noArrMethodError memberPos id
      
      Custom cls -> do
        info <- getClassInfo cls
        let err = noClsMethodError memberPos ownerType id
        methodInfo <- getMethodInfo err info id

        return (Member p owner $ debloat id, methodInfo)
        
      
      _ -> throwError $ noMethodError memberPos ownerType id
    
      where memberPos = position id

  S.Elem p v e -> throwError $ notAFuncError p var

  S.Null p -> throwError $ notAFuncError p var

  where

  getMethodInfo :: MonadError Error m
    => Error -> ClassInfo -> S.Ident -> m FuncInfo
  getMethodInfo err info id = case M.lookup (name id) (methods info) of
    Just methodInfo -> return methodInfo
    Nothing -> case parent info of
      Nothing         -> throwError err
      Just parentInfo -> getMethodInfo err parentInfo id


getCallableInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m FuncInfo
getCallableInfo v = do
  (_, info) <- getCallableVarAndInfo v
  return info

getCallableVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Var Func)
getCallableVar v = do
  (var, _) <- getCallableVarAndInfo v
  return var


-------------------------------------------------------------------------------
getAnyVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Any Var)
getAnyVar var = case var of
  S.Var p id -> do
    VarInfo x t <- getIdentInfo id
    
    return $ Any t $ Var p x

  S.Member p v id -> do
    
    Any ownerType owner <- getAnyVar v

    case ownerType of
      Arr t -> do
        unless (name id == lengthAttr)
          $ throwError $ noArrAttrError memberPos id
        return $ Any int $ Member p owner $ debloat id

      Custom cls -> do
        info <- getClassInfo cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwError $ noAttributeError memberPos ownerType id
          Just (VarInfo _ t) -> return $ Any t $ Member p owner $ debloat id
      
      _ -> throwError $ noAttributeError memberPos ownerType id

      where memberPos = position id

  S.Elem p v e -> do
    Any arrType arr <- getAnyVar v

    case arrType of
      Arr t -> do
        i <- getExpr int e
        return $ Any t $ Elem p arr i

      _ -> throwError $ notAnArrayArror (position e) v
  
  S.Null p -> return $ Any NullT $ Null p

getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> S.Var -> m (Var a)

getVar t var = do
  Any tt v <- getAnyVar var
  
  let err = wrongVarTypeError (position var) var t tt
  filterT err t tt v

getTypeOfVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m AnyType
getTypeOfVar var = do
  Any tt v <- getAnyVar var
  return $ AnyT tt



-------------------------------------------------------------------------------
getAnyExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Any Expr)
getAnyExpr expr = case expr of

  ---------------------------------------------------------------------
  S.EVar p v -> do
    Any t var <- getAnyVar v
    return $ Any t $ EVar p var
    
  S.ELitInt   p i -> return $ Any int   $ ELitInt   p (debloat i)
  S.ELitBool  p b -> return $ Any bool  $ ELitBool  p b
  S.EString   p s -> return $ Any str   $ EString   p (debloat s)

  ---------------------------------------------------------------------
  S.Neg p e -> do
    okExpr <- getExpr int e
    return $ Any int $ Neg p okExpr

  S.Not p e -> do
    okExpr <- getExpr bool e
    return $ Any bool $ Not p okExpr

  ---------------------------------------------------------------------
  S.EOp p op lhs rhs -> do
    okLHS <- getExpr int lhs
    okRHS <- getExpr int rhs
    return $ Any int $ EOp p (debloat op) okLHS okRHS

  S.ERel p op lhs rhs -> do
    AnyT lhsType <- getTypeOfExpr lhs

    okOp <- getOp lhsType op

    okLHS <- getExpr lhsType lhs
    okRHS <- getExpr lhsType rhs

    return $ Any bool $ ERel p okOp okLHS okRHS

  S.EBool p op lhs rhs -> do
    okLHS <- getExpr bool lhs
    okRHS <- getExpr bool rhs
    return $ Any bool $ EBool p (debloat op) okLHS okRHS

  ---------------------------------------------------------------------
  S.EApp p v args -> do
    FuncInfo _ retType paramTypes <- getCallableInfo v
    okV <- getCallableVar v
    okArgs <- validateArgs p v args paramTypes
    return $ Any retType $ EApp p okV okArgs
    
  ---------------------------------------------------------------------
  S.NewArr p elemType intExpr -> do
    i <- getExpr int intExpr

    case anyType elemType of
      AnyT elemT -> return $ Any (Arr elemT) (NewArr p elemT i)
  

  S.NewObj p clsId -> do
    cls <- getClass clsId
    return $ Any (Custom cls) (NewObj p (Custom cls))

  S.Cast p t e -> do
    Any eType okExpr <- getAnyExpr e
    case anyType t of
      AnyT tt -> return $ Any tt $ Cast p tt okExpr
    

getExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> S.Expr -> m (Expr a)
getExpr t expr = do
  Any tt okExpr <- getAnyExpr expr
  let err = wrongExprType (position okExpr) expr t tt
  filterT err t tt okExpr

getTypeOfExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m AnyType
getTypeOfExpr expr = do
  Any t okExpr <- getAnyExpr expr
  return $ AnyT t





getOp :: MonadError Error m => Type a -> S.RelOp -> m (RelOp a)
getOp t op = case (op, t) of
  (S.LTH p, Int _)  -> return $ LTH p
  (S.LE  p, Int _)  -> return $ LE  p
  (S.GTH p, Int _)  -> return $ GTH p
  (S.GE  p, Int _)  -> return $ GE  p
  (S.EQU p, _)      -> return $ EQU p
  (S.NE  p, _)      -> return $ NE  p
  
  (r, t) -> throwError $ wrongOpTypeError (position op) r t


validateArgs :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Var -> [S.Expr] -> [AnyType] -> m [Any Expr]
validateArgs p v args paramTypes = do
  if length args /= length paramTypes then
    throwError $ wrongNOParamsError p v (length paramTypes) (length args)

  else
    go (zip args paramTypes)

  where

    go :: (MonadState TypeCheckState m, MonadError Error m)
      => [(S.Expr, AnyType)] -> m [Any Expr]
    go [] = return []
    go ((expr, AnyT t) : rest) = do
      okExpr <- getExpr t expr
      go rest
