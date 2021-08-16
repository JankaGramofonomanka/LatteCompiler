{-# LANGUAGE
    RecordWildCards
  , FlexibleContexts
  , ConstraintKinds
  
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


type VarMap       = M.Map Label LocalVarMap
type LocalVarMap  = DM.DMap TypedIdent Value
type StrLitMap    = M.Map String SomeStrConst


type InheritanceMap = DM.DMap TypedIdent Reg

data PotentialBlock = PotBlock 
  { blockLabel  :: Label
  , inherited   :: InheritanceMap
  , blockBody   :: [SimpleInstr]
  , branchInstr :: Maybe BranchInstr
  , inputs      :: [Label]
  , outputs     :: [Label]
  , isEntry     :: Bool
  }

initPotBlock :: Label -> PotentialBlock
initPotBlock l = PotBlock 
  { blockLabel  = l
  , inherited   = DM.empty
  , blockBody   = []
  , branchInstr = Nothing
  , inputs      = []
  , outputs     = []
  , isEntry     = False
  }

data PotentialFunc where
  PotFunc ::
    { label       :: FuncLabel (GetPrimType t) ts
    , retType     :: Sing t
    , argTypes    :: Sing ts
    , args        :: ParamList ts
    , body        :: M.Map Label PotentialBlock
    , blockOrder  :: [Label]
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

    , currentBlockLabel :: Maybe Label
    , currentFunc       :: Maybe PotentialFunc
    , currentProg       :: PotentialProg
    , currentScopeLevel :: Int
    } -> LLVMState

emptyState :: LLVMState
emptyState = LLVMState
  { regCounter = M.empty
  , constCounter = M.empty
  , labelCounter = M.empty

  , varMap        = M.empty
  , strLitMap     = M.empty

  , currentBlockLabel = Nothing
  , currentFunc       = Nothing
  , currentProg       = PotProg { mainFunc = Nothing, funcs = [] }
  , currentScopeLevel = 0
  }

type LLVMConverter m = (MonadState LLVMState m, MonadError Error m)

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

putCurrentBlockLabel :: MonadState LLVMState m => Label-> m ()
putCurrentBlockLabel l = do
  LLVMState { currentBlockLabel = _, .. } <- get
  put $ LLVMState { currentBlockLabel = Just l, .. }

putCurrentFunc :: MonadState LLVMState m => PotentialFunc -> m ()
putCurrentFunc f = do
  LLVMState { currentFunc = _, .. } <- get
  put $ LLVMState { currentFunc = Just f, .. }

putCurrentProg :: MonadState LLVMState m => PotentialProg -> m ()
putCurrentProg prog = do
  LLVMState { currentProg = _, .. } <- get
  put $ LLVMState { currentProg = prog, .. }

dropCurrentBlockBlock :: MonadState LLVMState m => m ()
dropCurrentBlockBlock = do
  LLVMState { currentBlockLabel = _, .. } <- get
  put $ LLVMState { currentBlockLabel = Nothing, .. }

putLocalVarMap :: MonadState LLVMState m => Label -> LocalVarMap -> m ()
putLocalVarMap l localM = do
  m <- gets varMap
  putVarMap $ M.insert l localM m

putPotBlock :: LLVMConverter m
  => PotentialBlock -> m ()
putPotBlock block@PotBlock { blockLabel = l, .. } = do
  PotFunc { body = body, .. } <- getCurrentFunc
  putCurrentFunc $ PotFunc { body = M.insert l block body, .. }

putBlockOrder :: LLVMConverter m => [Label] -> m ()
putBlockOrder newOrder = do
  PotFunc { blockOrder = order, .. } <- getCurrentFunc
  putCurrentFunc $ PotFunc { blockOrder = newOrder, .. }


-- getters --------------------------------------------------------------------


getCurrentBlockLabel :: LLVMConverter m => m Label
getCurrentBlockLabel = do
  mbLabel <- gets currentBlockLabel
  case mbLabel of
    Nothing -> throwError internalNoCurrentBlockError
    Just l -> return l
    

getCurrentFunc :: LLVMConverter m => m PotentialFunc
getCurrentFunc = do
  mbFunc <- gets currentFunc
  case mbFunc of
    Just bl -> return bl
    Nothing -> throwError internalNoFuncError


getPotBlock :: LLVMConverter m => Label -> m PotentialBlock
getPotBlock l = do
  PotFunc { body = body, .. } <- getCurrentFunc
  case M.lookup l body of
    Nothing -> throwError internalNoSuchBlockError
    Just bl -> return bl

getPotBlock' :: LLVMConverter m => Label -> m PotentialBlock
getPotBlock' l = do
  PotFunc { body = body, .. } <- getCurrentFunc
  case M.lookup l body of
    Just bl -> return bl
    Nothing -> do
      let block = initPotBlock l
      putPotBlock block
      return block


getCurrentBlock :: LLVMConverter m => m PotentialBlock
getCurrentBlock = getCurrentBlockLabel >>= getPotBlock


getLocalVarMap :: MonadState LLVMState m => Label -> m LocalVarMap
getLocalVarMap l = do
  m <- gets varMap
  case M.lookup l m of
    Just mm -> return mm
    Nothing -> putLocalVarMap l DM.empty >> return DM.empty

getCurrentVarMap :: LLVMConverter m => m LocalVarMap
getCurrentVarMap = getCurrentBlockLabel >>= getLocalVarMap

putCurrentVarMap :: LLVMConverter m => LocalVarMap -> m ()
putCurrentVarMap m = do
  l <- getCurrentBlockLabel
  putLocalVarMap l m
  
incrScopeLevel :: MonadState LLVMState m => m ()
incrScopeLevel = do
  LLVMState { currentScopeLevel = l, .. } <- get
  put $ LLVMState { currentScopeLevel = l + 1, .. }

decrScopeLevel :: LLVMConverter m => m ()
decrScopeLevel = do
  LLVMState { currentScopeLevel = l, .. } <- get
  when (l <= 0) $ throwError internalScopeLevelBelowZeroError
  put $ LLVMState { currentScopeLevel = l - 1, .. }


getBlockOrder :: LLVMConverter m => m [Label]
getBlockOrder = blockOrder <$> getCurrentFunc


