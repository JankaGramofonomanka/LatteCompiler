{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RecordWildCards      #-}

module TypeCheck.State where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import qualified Syntax.Syntax as S
import Syntax.SyntaxGADT
import Position.Position
import Errors
import Position.SyntaxPosition
import Position.SyntaxGADTPosition
import LangElemClasses
import Syntax.Debloater
import qualified Scope as Sc


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


type VarMap = M.Map String VarInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo
type VarScope = Sc.Scope String VarInfo

data VarInfo where
  VarInfo :: { varId :: Ident a, varType :: Type a } -> VarInfo

data FuncInfo where
  FuncInfo :: {
    funcId :: FuncIdent,
    retType :: Type a,
    paramTypes :: [AnyType]
  } -> FuncInfo

data ClassInfo = ClassInfo {
  classId :: ClassIdent,
  parent :: Maybe ClassInfo,
  attributes :: VarMap,
  methods :: FuncMap
}

data TypeCheckState = TypeCheckState { 
  varScope :: VarScope,
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











