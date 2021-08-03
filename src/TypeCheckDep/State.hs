{-# LANGUAGE 
    RecordWildCards
  , FlexibleContexts

  , GADTs
  , DataKinds
#-}

module TypeCheckDep.State where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Prelude
import Data.Singletons.Sigma

import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Position.Position
import Errors
import Position.SyntaxPosition
import Position.SyntaxDepPosition
import LangElemClasses
import Syntax.Debloater
import qualified Scope as Sc

import Dependent

{-
int :: Type Int
int = Int fakePos
bool :: Type Bool
bool = Bool fakePos
str :: Type String
str = Str fakePos
void :: Type Void
void = Void fakePos
-}



lengthAttr :: String
lengthAttr = "length"


type VarMap = M.Map String VarInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo
type VarScope = Sc.Scope String VarInfo
type FuncScope = Sc.Scope String FuncInfo

data VarInfo where
  VarInfo :: { varId :: Ident t, varType :: SLatteType t } -> VarInfo

data FuncInfo where
  FuncInfo :: {
    funcId      :: FuncIdent t ts,
    retType     :: SLatteType t,
    paramTypes  :: SList ts
  } -> FuncInfo

data ClassInfo where 
  ClassInfo :: {
    classId     :: ClassIdent cls,
    parent      :: Maybe ClassInfo,
    attributes  :: VarMap,
    methods     :: FuncMap
  } -> ClassInfo

data TypeCheckState = TypeCheckState { 
  varScope    :: VarScope,
  funcScope   :: FuncScope,
  classMap    :: ClassMap,
  returnType  :: Maybe (Some SLatteType),
  selfType    :: Maybe SomeCustomType
}

type SomeCustomType = Sigma Natural (TyCon1 (ExtractParam2 SLatteType Custom))

emptyState :: TypeCheckState
emptyState = TypeCheckState { 
  varScope    = Sc.subScope Sc.EmptyScope,
  funcScope   = Sc.subScope Sc.EmptyScope,
  classMap    = M.empty,
  returnType  = Nothing,
  selfType    = Nothing
}












