{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module TypeCheck.State where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import qualified Syntax.Syntax as S
import Syntax.SyntaxGADT
import Position
import Errors
import Syntax.SyntaxPosition
import Syntax.SyntaxGADTPosition
import LangElemClasses
import Syntax.Debloater


int :: Type Int
int = Int fakePos
bool :: Type Bool
bool = Bool fakePos
str :: Type String
str = Str fakePos


anyType :: S.Type -> AnyType
anyType t = case t of
  S.Int   p         -> AnyT $ Int p
  S.Str   p         -> AnyT $ Int p
  S.Bool  p         -> AnyT $ Int p
  S.Void  p         -> AnyT $ Int p
  S.Arr   elemType  -> case anyType elemType of
                        AnyT tt -> AnyT $ Arr tt

  S.Custom cls      -> AnyT $ Custom (debloat cls)

lengthAttr :: String
lengthAttr = "length"

throwTODO :: MonadError Error m => m a
throwTODO = throwError $ SimpleError "TODO"

type IdentMap = M.Map String IdentInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo

data IdentInfo where
  IdentInfo :: { varId :: Ident a, varType :: Type a } -> IdentInfo

data FuncInfo where
  FuncInfo :: {
    funcId :: FuncIdent,
    retType :: Type a,
    paramTypes :: [Any Type]
  } -> FuncInfo

data ClassInfo = ClassInfo {
  classId :: ClassIdent,
  parent :: Maybe ClassInfo,
  attributes :: IdentMap,
  methods :: FuncMap
}

data TypeCheckState = TypeCheckState { 
  idMap :: IdentMap,
  funcMap :: FuncMap,
  classMap :: ClassMap
}

newtype X1 (a :: * -> *) b = X1 (a b)
newtype X2 (a :: * -> *) (b :: * -> *) c = X2 (a (b c))
toX2 :: e (a b) -> X2 e a b
toX2 = X2

fromX2 :: X2 e a b -> e (a b)
fromX2 (X2 x) = x

filterT :: (MonadError Error m) => Error -> Type a -> Type b -> e b -> m (e a)
filterT _ (Int _)  (Int _)  x = return x
filterT _ (Str _)  (Str _)  x = return x
filterT _ (Bool _) (Bool _) x = return x
filterT err (Arr t1) (Arr t2) x = do
  xx <- filterT err t1 t2 (toX2 x)
  
  return $ fromX2 xx

filterT err (Custom id1) (Custom id2) x = do
  if id1 == id2 then
    return x
  else
    throwError err

filterT err expected actual x = throwError err

filterST :: (MonadError Error m) => Error -> Type a -> S.Type -> m (Type a)
filterST _ t@(Int _)  (S.Int _)   = return t
filterST _ t@(Str _)  (S.Str _)   = return t
filterST _ t@(Bool _) (S.Bool _)  = return t
filterST err (Arr t1) (S.Arr t2) = do
  t <- filterST err t1 t2
  
  return $ Arr t

filterST err t@(Custom id1) (S.Custom id2) = do
  if name id1 == name id2 then
    return t
  else
    throwError err

filterST err expected actual = throwError err



getIdentInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m IdentInfo
getIdentInfo id = do
  idMap <- gets idMap
  case M.lookup (name id) idMap of
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
  (IdentInfo x actT) <- getIdentInfo id
  let err = wrongVarTypeError (position id) id expT actT 
  filterT err expT actT x


getFuncInfo :: 
  ( MonadState TypeCheckState m,
    MonadError Error m,
    IsIdent i,
    HasPosition i
  )
  => i -> m FuncInfo
getFuncInfo id = do
  idMap <- gets funcMap
  case M.lookup (name id) idMap of
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
  idMap <- gets classMap
  case M.lookup (name id) idMap of
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
  S.Var p id -> throwError $ notAVarError p id

  S.Fun p id -> do
    info <- getFuncInfo id
    
    return (Var p (funcId info), info)

  S.Member p v id -> do
    
    AnyT vType <- getTypeOfVar v
    owner <- getVar vType v
    
    -- TODO a redundant code
    case vType of

      Arr _ -> throwError $ noArrMethodError mamberPos id
      
      Custom cls -> do
        info <- getClassInfo cls
        case M.lookup (name id) (methods info) of
          Nothing -> throwError $ noClsMethodError mamberPos vType id
          Just methodInfo -> return (Member p owner $ debloat id, methodInfo)
      
      _ -> throwError $ noMethodError mamberPos vType id
    
      where mamberPos = position id

  S.Elem p v e -> throwError $ notAFuncError p var

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
-- TODO - write getAnyVar and use it in getVar and getTypeOfVar
getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> S.Var -> m (Var a)
getVar t var = case var of
  S.Var p id -> do
    x <- getIdent t id
    return $ Var p x

  S.Fun p id -> throwError $ notAVarError p id

  S.Member p v id -> do
    
    AnyT vType <- getTypeOfVar v
    owner <- getVar vType v
    
    -- TODO a lot of redundant code
    case vType of
      Arr t -> do
        unless (name id == lengthAttr)
          $ throwError $ noArrAttrError mamberPos id
        return $ Member p owner $ debloat id


      Custom cls -> do
        info <- getClassInfo cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwError $ noAttributeError mamberPos vType id
          Just (IdentInfo _ t) -> return $ Member p owner $ debloat id
      
      _ -> throwError $ noAttributeError mamberPos vType id
    
      where mamberPos = position id

  S.Elem p v e -> do
    arr <- getVar (Arr t) v
    i <- getExpr int e
    
    return $ Elem p arr i


getTypeOfVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var
  -> m AnyType
getTypeOfVar v = case v of
  S.Var p id -> do
    IdentInfo x t <- getIdentInfo id
    return $ AnyT t

  S.Fun p id -> throwError $ notAVarError p id

  S.Member p v id -> do
    AnyT vType <- getTypeOfVar v

    -- TODO a lot of redundant code
    case vType of
      Arr t -> do
        unless (name id == lengthAttr)
          $ throwError $ noArrAttrError memberPos id
        return $ AnyT int

      Custom cls -> do
        info <- getClassInfo cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwError $ noAttributeError memberPos vType id
          Just (IdentInfo _ t) -> return $ AnyT t
      
      _ -> throwError $ noAttributeError memberPos vType id

      where memberPos = position id

  S.Elem p v e -> do
    AnyT vType <- getTypeOfVar v

    case vType of
      Arr t -> do
        i <- getExpr int e
        return $ AnyT t

      _ -> throwError $ notAnArrayArror (position e) v




-------------------------------------------------------------------------------
-- TODO - write getAnyexpr and use it in getExpr and getTypeOfExpr
getExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => Type a -> S.Expr -> m (Expr a)
getExpr t expr = case expr of

  ---------------------------------------------------------------------
  S.EVar p v -> do
    var <- getVar t v
    return $ EVar p var
    

  S.ELitInt p i -> do
    let err = wrongLitTypeError p i t int
    filterT err t int $ ELitInt p ii
    
    where ii = debloat i
    
  S.ELitBool p b -> do
    let err = wrongLitTypeError p b t bool
    filterT err t bool $ ELitBool p b

  S.EString p s -> do
    let err = wrongLitTypeError p s t bool
    filterT err t str $ EString p ss

    where ss = debloat s

  ---------------------------------------------------------------------
  S.Neg p e -> do
    okExpr <- getExpr int e

    let err = wrongExprType p expr t int
    filterT err t int $ Neg p okExpr

  S.Not p e -> do
    okExpr <- getExpr bool e

    let err = wrongExprType p expr t bool
    filterT err t bool $ Not p okExpr

  ---------------------------------------------------------------------
  S.EOp p op lhs rhs -> do
    okLHS <- getExpr int lhs
    okRHS <- getExpr int rhs

    let err = wrongExprType p expr t int
    filterT err t int $ EOp p (debloat op) okLHS okRHS


  S.ERel p op lhs rhs -> do
    AnyT lhsType <- getTypeOfExpr lhs

    okOp <- getOp lhsType op

    okLHS <- getExpr lhsType lhs
    okRHS <- getExpr lhsType rhs

    let err = wrongExprType p expr t lhsType
    filterT err t bool $ ERel p okOp okLHS okRHS

  S.EBool p op lhs rhs -> do
    okLHS <- getExpr bool lhs
    okRHS <- getExpr bool rhs

    let err = wrongExprType p expr t bool
    filterT err t bool $ EBool p (debloat op) okLHS okRHS
  
  ---------------------------------------------------------------------
  S.EApp p v args -> do
    FuncInfo _ retType paramTypes <- getCallableInfo v
    okV <- getCallableVar v
    okArgs <- validateArgs p v args paramTypes
    
    let err = wrongExprType p expr t retType
    okRetType <- filterT err t retType retType

    return $ EApp p okV okArgs
  
  ---------------------------------------------------------------------
  S.NewArr p elemType intExpr -> do
    i <- getExpr int intExpr

    let err = wrongExprType p expr t (S.Arr elemType)
    filterST err t (S.Arr elemType)

    case t of
      Arr elemT -> return $ NewArr p elemT i
      _ -> throwError err
      

  S.NewObj p clsId -> do
    cls <- getClass clsId

    let err = wrongExprType p expr t (Custom cls)
    filterT err t (Custom cls) $ NewObj p (Custom cls)

  S.Cast p tt e -> do
    AnyT eType <- getTypeOfExpr e
    okExpr <- getExpr eType e
    
    let err = wrongExprType p expr t tt
    ttt <- filterST err t tt
    
    return $ Cast p ttt okExpr


getTypeOfExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m AnyType
getTypeOfExpr expr = case expr of

  ---------------------------------------------------------------------
  S.EVar p v      -> getTypeOfVar v    
  S.ELitInt p i   -> return $ AnyT int
  S.ELitBool p b  -> return $ AnyT bool
  S.EString p s   -> return $ AnyT str

  ---------------------------------------------------------------------
  S.Neg p e -> do
    getExpr int e
    return $ AnyT int
    
  S.Not p e -> do
    getExpr bool e
    return $ AnyT bool

  ---------------------------------------------------------------------
  S.EOp p op lhs rhs -> do
    getExpr int lhs
    getExpr int rhs

    return $ AnyT int


  S.ERel p op lhs rhs -> do
    AnyT lhsType <- getTypeOfExpr lhs

    getOp lhsType op
    getExpr lhsType lhs
    getExpr lhsType rhs

    return $ AnyT bool

  S.EBool p op lhs rhs -> do
    getExpr bool lhs
    getExpr bool rhs

    return $ AnyT bool
  
  ---------------------------------------------------------------------
  S.EApp p v args -> do
    FuncInfo _ retType paramTypes <- getCallableInfo v
    getCallableVar v
    validateArgs p v args paramTypes
    
    return $ AnyT retType
  
  ---------------------------------------------------------------------
  S.NewArr p elemType intExpr -> do
    getExpr int intExpr

    case anyType elemType of
      AnyT elemT -> return $ AnyT $ Arr elemT


  S.NewObj p clsId -> do
    cls <- getClass clsId

    return $ AnyT (Custom cls)


  S.Cast p tt e -> do
    AnyT eType <- getTypeOfExpr e
    getExpr eType e
    
    case anyType tt of
      AnyT ttt -> return $ AnyT ttt




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
  => Pos -> S.Var -> [S.Expr] -> [Any Type] -> m [Any Expr]
validateArgs p v args paramTypes = do
  if length args /= length paramTypes then
    throwError $ wrongNOParamsError p v (length paramTypes) (length args)

  else
    go (zip args paramTypes)

  where

    go :: (MonadState TypeCheckState m, MonadError Error m)
      => [(S.Expr, Any Type)] -> m [Any Expr]
    go [] = return []
    go ((expr, Any _ t) : rest) = do
      okExpr <- getExpr t expr
      go rest

