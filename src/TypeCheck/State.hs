{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
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
void :: Type Void
void = Void fakePos




lengthAttr :: String
lengthAttr = "length"


type VarMap = M.Map String VarInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo
type VarScope = Sc.Scope String VarInfo
type FuncScope = Sc.Scope String FuncInfo

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
  funcScope :: FuncScope,
  classMap :: ClassMap,
  returnType :: Maybe AnyType
}

emptyState :: TypeCheckState
emptyState = TypeCheckState { 
  varScope    = Sc.subScope Sc.EmptyScope,
  funcScope   = Sc.subScope Sc.EmptyScope,
  classMap    = M.empty,
  returnType  = Nothing
}












