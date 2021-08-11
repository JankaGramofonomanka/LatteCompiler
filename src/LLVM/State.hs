{-# LANGUAGE
    RecordWildCards
  , FlexibleContexts
  
  , DataKinds
  , ScopedTypeVariables
  , KindSignatures
  , TypeApplications
  , TypeOperators
  , TypeFamilies
  , GADTs
  , StandaloneKindSignatures
#-}
module LLVM.State where


import Prelude hiding ( EQ )
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Singletons
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM
import qualified Data.Some as D

import LLVM.LLVM
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent



  
  

type RegCountMap = M.Map String Int
type ConstCountMap = M.Map String Int
type LabelCountMap = M.Map String Int
type BlockInfoMap = M.Map Label BlockInfo

type VarMap       = M.Map Label LocalVarMap
type LocalVarMap  = DM.DMap TypedIdent Value
type StrLitMap    = M.Map String SomeStrConst

data BlockInfo
  = BlockInfo
    { inputs :: [Label]
    , outputs :: [Label]
    , scopeLevel :: Int
    }
    
emptyBlockInfo :: Int -> BlockInfo
emptyBlockInfo = BlockInfo [] []

type InheritanceMap = DM.DMap TypedIdent Reg

data PotentialBlock = PotBlock 
  { blockLabel  :: Label
  , inherited   :: InheritanceMap
  , blockBody   :: [SimpleInstr]
  }

data PotentialFunc where
  PotFunc ::
    { label     :: FuncLabel (GetPrimType t) ts
    , retType   :: Sing t
    , argTypes  :: Sing ts
    , args      :: ArgList ts
    , body      :: M.Map Label (InheritanceMap, SimpleBlock)
    } -> PotentialFunc

data PotentialProg where
  PotProg ::
    { mainFunc :: Maybe (Func (I 32) '[])
    , funcs :: [SomeFunc]
    } -> PotentialProg


data LLVMState where 
  LLVMState ::
    { regCounter    :: RegCountMap
    , constCounter  :: ConstCountMap
    , labelCounter  :: LabelCountMap

    , varMap        :: VarMap
    , strLitMap     :: StrLitMap
    , blockInfoMap  :: BlockInfoMap

    , currentBlock      :: Maybe PotentialBlock
    , currentFunc       :: PotentialFunc
    , currentProg       :: PotentialProg
    , currentScopeLevel :: Int
    } -> LLVMState


-- putters --------------------------------------------------------------------
putRegCounter :: MonadState LLVMState m => RegCountMap -> m ()
putRegCounter m = do
  LLVMState { regCounter = _, .. } <- get
  put $ LLVMState { regCounter = m, .. }

putConstCounter :: MonadState LLVMState m => ConstCountMap -> m ()
putConstCounter m = do
  LLVMState { constCounter = _, .. } <- get
  put $ LLVMState { constCounter = m, .. }

putLabelCounter :: MonadState LLVMState m => LabelCountMap -> m ()
putLabelCounter n = do
  LLVMState { labelCounter = _, .. } <- get
  put $ LLVMState { labelCounter = n, .. }

putVarMap :: MonadState LLVMState m => VarMap -> m ()
putVarMap m = do
  LLVMState { varMap = _, .. } <- get
  put $ LLVMState { varMap = m, .. }

putStrLitMap :: MonadState LLVMState m => StrLitMap -> m ()
putStrLitMap m = do
  LLVMState { strLitMap = _, .. } <- get
  put $ LLVMState { strLitMap = m, .. }

putBlockInfoMap :: MonadState LLVMState m => BlockInfoMap -> m ()
putBlockInfoMap m = do
  LLVMState { blockInfoMap = _, .. } <- get
  put $ LLVMState { blockInfoMap = m, .. }

putCurrentBlock :: MonadState LLVMState m => PotentialBlock -> m ()
putCurrentBlock b = do
  LLVMState { currentBlock = _, .. } <- get
  put $ LLVMState { currentBlock = Just b, .. }

putCurrentFunc :: MonadState LLVMState m => PotentialFunc -> m ()
putCurrentFunc f = do
  LLVMState { currentFunc = _, .. } <- get
  put $ LLVMState { currentFunc = f, .. }

putCurrentProg :: MonadState LLVMState m => PotentialProg -> m ()
putCurrentProg prog = do
  LLVMState { currentProg = _, .. } <- get
  put $ LLVMState { currentProg = prog, .. }


dropCurrentBlockBlock :: MonadState LLVMState m => m ()
dropCurrentBlockBlock = do
  LLVMState { currentBlock = _, .. } <- get
  put $ LLVMState { currentBlock = Nothing, .. }

putBlockInfo :: MonadState LLVMState m => Label -> BlockInfo -> m ()
putBlockInfo label info = do
  m <- gets blockInfoMap
  putBlockInfoMap $ M.insert label info m

putLocalVarMap :: MonadState LLVMState m => Label -> LocalVarMap -> m ()
putLocalVarMap l localM = do
  m <- gets varMap
  putVarMap $ M.insert l localM m

