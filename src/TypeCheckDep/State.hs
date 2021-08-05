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

import Data.Singletons.Prelude hiding ( Error )
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
int :: SLatteType TInt
int = STInt
bool :: SLatteType TBool
bool = STBool
str :: SLatteType TStr
str = STStr
void :: SLatteType TVoid
void = STVoid
null :: SLatteType TNull
null = STNull
-- -}



lengthAttr :: String
lengthAttr = "length"


type VarMap = M.Map String VarInfo
type FuncMap = M.Map String FuncInfo
type ClassMap = M.Map String ClassInfo
type ClassNameMap = M.Map ClassId String
--type ClassIdMap = M.Map String ClassId
--type ClassMap = M.Map ClassId ClassInfo

type VarScope = Sc.Scope String VarInfo
type FuncScope = Sc.Scope String FuncInfo

data VarInfo where
  VarInfo :: { varId :: Ident t, varType :: SLatteType t } -> VarInfo

data FuncInfo where
  FuncInfo :: 
    { funcId      :: FuncIdent t ts
    , retType     :: SLatteType t
    , paramTypes  :: SList ts
    } -> FuncInfo

data CallableInfo where
  CallableInfo :: 
    { callableId  :: Callable t ts
    , cRetType    :: SLatteType t
    , cRaramTypes :: SList ts
    } -> CallableInfo

data ClassInfo where 
  ClassInfo :: 
    { classId     :: SClassId cls
    , parent      :: Maybe ClassInfo
    , attributes  :: VarMap
    , methods     :: FuncMap
    , declaredAt  :: Pos
    } -> ClassInfo

data TypeCheckState = TypeCheckState 
  { varScope      :: VarScope
  , funcScope     :: FuncScope
  , classMap      :: ClassMap
  , classNameMap  :: ClassNameMap
  , returnType    :: Maybe (Some SLatteType)
  , selfType      :: Maybe SomeCustomType
  , currentPos    :: Pos
  , classCounter  :: Natural
  }

type SomeCustomType = Sigma Natural (TyCon1 (ExtractParam2 SLatteType Custom))

emptyState :: TypeCheckState
emptyState = TypeCheckState 
  { varScope      = Sc.subScope Sc.EmptyScope
  , funcScope     = Sc.subScope Sc.EmptyScope
  , classMap      = M.empty
  , classNameMap  = M.empty
  , returnType    = Nothing
  , selfType      = Nothing
  , currentPos    = (0, 0)
  , classCounter  = Zero
  }








