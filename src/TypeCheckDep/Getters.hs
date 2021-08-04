{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
  , KindSignatures
  , DataKinds
  , PolyKinds
#-}

module TypeCheckDep.Getters where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error, Any )

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors
import LangElemClasses
import Syntax.DebloaterDep
import qualified Scope as Sc
import TypeCheckDep.State
import Dependent


someType :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Type -> m (Some SLatteType)
someType t = case t of
    S.Int  p    -> return $ Some STInt
    S.Str  p    -> return $ Some STStr
    S.Bool p    -> return $ Some STBool
    S.Void p    -> return $ Some STVoid
    S.Arr elemT -> do
      Some elemTT <- someType elemT
      return $ Some $ SArr elemTT

    S.Custom id -> do
      ClassInfo { classId = clsId, .. } <- getClassInfo id
      return $ Some $ SCustom clsId

filterNatural :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> SNatural a -> SNatural b -> e b -> m (e a)
filterNatural _ SZero SZero x = return x
filterNatural err (SSucc n1) (SSucc n2) x = do
  xx <- filterNatural err n1 n2 (extractParam2 x)
  return $ insertParam2 xx
filterNatural err _ _ _ = throwError err

filterT :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> SLatteType a -> SLatteType b -> e b -> m (e a)
filterT _ STInt   STInt   x = return x
filterT _ STStr   STStr   x = return x
filterT _ STBool  STBool  x = return x
filterT err (SArr t1) (SArr t2) x = do
  xx <- filterT err t1 t2 (extractParam2 x)
  
  return $ insertParam2 xx

filterT err (SCustom id1) (SCustom id2) x = do
  
    --assertSubClass err id1 id2
    --return $ forceCast x

    -- TODO no auto casting of subclasses
    xx <- filterNatural err id1 id2 (extractParam2 x)
    return $ insertParam2 xx


filterT err expected actual x = throwError err



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
  => SLatteType a -> i -> m (Ident a)
getIdent expT id = do
  (VarInfo x actT) <- getIdentInfo id

  expKW <- getTypeKW (position id) expT
  actKW <- getTypeKW (position id) actT
  let err = wrongIdentTypeError (position id) id expKW actKW
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

getClassIdent :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SClassId cls -> m (ClassIdent cls)
getClassIdent p cls = do
  m <- gets classNameMap
  case M.lookup (fromSing cls) m of
    Nothing -> throwError $ internalNoClassError p
    Just s -> return $ ClassIdent p s

--getBloatedIdent :: (MonadState TypeCheckState m, MonadError Error m)
--  => Pos -> SClassId cls -> m (ClassIdent cls)

getTypeKW :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> SLatteType t -> m (TypeKW t)
getTypeKW p t = case t of
  STInt       -> return $ KWInt p
  STStr       -> return $ KWStr p
  STBool      -> return $ KWBool p
  STVoid      -> return $ KWVoid p
  SArr elemT  -> do
    elemTT <- getTypeKW p elemT
    return $ KWArr elemTT

  SCustom cls -> do
    clsId <- getClassIdent p cls
    return $ KWCustom clsId
  
  STNull       -> throwError $ internallNullKWError p

someTypeKW :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Type -> m (Some TypeKW)
someTypeKW p t = do
  Some tt <- someType t
  typeKW <- getTypeKW p tt
  return $ Some typeKW

{-
getClassInfoById :: 
  ( MonadState TypeCheckState m, MonadError Error m)
  => ClassId -> m ClassInfo
getClassInfoById id = do
  m <- gets classNameMap
  case M.lookup id m of
    Nothing -> undefined
    Just clsName -> do
-- -}


{-
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
-- -}

{-
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
-- -}

getSelfType ::
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => Pos -> m SomeCustomType
getSelfType p = do
  maybeSelfType <- gets selfType
  case maybeSelfType of
    Nothing -> throwError $ selfOutsideClassError p
    Just t -> return t

-------------------------------------------------------------------------------

{-
isParent :: (MonadState TypeCheckState m, MonadError Error m)
  => SClassId cls1 -> SClassId cls2 -> m Bool
isParent cls1 cls2 = do
  if cls1 == cls2 then
    return True
  else do
    ClassInfo { parent = mbParent, .. } <- getClassInfo cls2
    case mbParent of
      Nothing -> return False
      Just (ClassInfo parentId _ _ _) -> isParent cls1 parentId


assertSubClass :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> SClassId cls1 -> SClassId cls2 -> m ()
assertSubClass err parent child = do
  success <- isParent parent child
  if success then
    return ()
  else throwError err

assertRetTypeIsSomething :: 
  ( MonadState TypeCheckState m,
    MonadError Error m
  )
  => Pos -> m ()
assertRetTypeIsSomething p = do
  maybeRetType <- gets returnType
  when (isNothing maybeRetType) $ throwError $ internalNoReturnTypeError p

getCommonType :: (MonadState TypeCheckState m, MonadError Error m)
  => Error -> Error -> SLatteType a -> SLatteType b -> m (Some SLatteType)
getCommonType err errCls t1 t2 = case (t1, t2) of
  (SCustom cls1, SCustom cls2) -> do
    cls1IsParent <- isParent cls1 cls2
    cls2IsParent <- isParent cls2 cls1

    if cls1IsParent then
      return $ Some t1
    else if cls2IsParent then
      return $ Some t2
    else
      throwError errCls
  
  _ -> do
    t <- filterT err t1 t2 t2
    return $ Some t
-- -}

-------------------------------------------------------------------------------
type SomeCollable = Sigma2 LatteType [LatteType] (TyCon2 Callable)

-- {-
getCallableInfo :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m CallableInfo
getCallableInfo var = case var of

  S.Var p id -> do
    FuncInfo funcId retType paramTypes <- getFuncInfo id
    
    return $ CallableInfo (Func p funcId) retType paramTypes

  S.Member p e id -> do
    
    ownerType :&: owner <- getAnyExpr e

    ownerTKW <- getTypeKW p ownerType
    
    case ownerType of

      SArr _ -> throwError $ noArrMethodError memberPos id
      
      SCustom clsId -> do

        cls <- getClassIdent p clsId
        info <- getClassInfo cls
        let err = noClsMethodError memberPos ownerTKW id
        FuncInfo methId retType paramTypes <- getMethodInfo err info id

        return $ CallableInfo (Method p owner methId) retType paramTypes
        
      
      _ -> throwError $ noMethodError memberPos ownerTKW id
    
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

-- TODO - redundant
getCallableVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m SomeCollable
getCallableVar v = do
  CallableInfo var retType paramTypes <- getCallableInfo v

  return $ (retType, paramTypes) :&&: var


-------------------------------------------------------------------------------
getAnyVar :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Var -> m (Any Var)
getAnyVar var = case var of
  S.Var p id -> do
    VarInfo x t <- getIdentInfo id
    
    return $ t :&: Var p x

  S.Member p e id -> do
    
    ownerType :&: owner <- getAnyExpr e

    ownerTKW <- getTypeKW p ownerType

    case ownerType of
      SArr t -> do
        unless (name id == lengthAttr)
          $ throwError $ noArrAttrError memberPos id
        return $ STInt :&: Attr p owner (debloat id)

      
      SCustom clsId -> do

        cls <- getClassIdent p clsId
        info <- getClassInfo cls
        case M.lookup (name id) (attributes info) of
          Nothing -> throwError $ noAttributeError memberPos ownerTKW id
          Just (VarInfo _ t) -> return $ t :&: Attr p owner (debloat id)
      
      _ -> throwError $ noAttributeError memberPos ownerTKW id

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
    n :&: selfT <- getSelfType p
    return $ insertParam2 selfT :&: Self p

getVar :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.Var -> m (Var a)

getVar t var = do
  tt :&: v <- getAnyVar var
  
  kwT <- getTypeKW (position var) t
  kwTT <- getTypeKW (position var) tt
  let err = wrongVarTypeError (position var) var kwT kwTT
  filterT err t tt v

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
      opKW <- getTypeKW p operandType
      case operandType of
        STStr -> getStrOpExpr p lhs rhs
        STInt -> getIntOpExpr p op lhs rhs
        _     -> throwError
                  $ exprTypeNotInListError p expr expectedList opKW

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
    
    lhsKW <- getTypeKW (position lhs) lhsType
    rhsKW <- getTypeKW (position rhs) rhsType
    let err = wrongExprTypeError p rhs lhsKW rhsKW
    let errCls = typesNotCompatibileError p lhs rhs lhsKW rhsKW
    
    
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
getExpr t expr = do
  tt :&: okExpr <- getAnyExpr expr
  
  kwT <- getTypeKW (position expr) t
  kwTT <- getTypeKW (position expr) tt
  let err = wrongExprTypeError (position okExpr) expr kwT kwTT
  filterT err t tt okExpr

getTypeOfExpr :: (MonadState TypeCheckState m, MonadError Error m)
  => S.Expr -> m (Some SLatteType)
getTypeOfExpr expr = do
  t :&: okExpr <- getAnyExpr expr
  return $ Some t





getOp :: (MonadState TypeCheckState m, MonadError Error m)
  => SLatteType a -> S.RelOp -> m (RelOp a)
getOp t op = case (op, t) of
  (S.LTH p, STInt)  -> return $ LTH p
  (S.LE  p, STInt)  -> return $ LE  p
  (S.GTH p, STInt)  -> return $ GTH p
  (S.GE  p, STInt)  -> return $ GE  p
  (S.EQU p, _)      -> return $ EQU p
  (S.NE  p, _)      -> return $ NE  p
  
  (r, t) -> do
    kwT <- getTypeKW fakePos t
    throwError $ wrongOpTypeError (position op) r kwT



validateArgs :: (MonadState TypeCheckState m, MonadError Error m)
  => Pos -> S.Var -> [S.Expr] -> SList ts -> m (ExprList ts)
validateArgs p v [] SNil = return DNil

validateArgs p v (expr : exprs) (SCons t ts) = do
  okExpr <- getExpr t expr
  okExprs <- validateArgs p v exprs ts
  return $ okExpr :> okExprs
      
validateArgs p v args paramTypes
  = throwError $ wrongNOParamsError p v (sLengthInt paramTypes) (length args)

