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
import Data.Singletons.Prelude hiding ( Error )
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM
import Data.GADT.Compare

import LLVM.LLVM
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent
import SingChar
import LLVM.LLVM (FuncLabel(FuncLabel))


type RegCountMap = M.Map String Int
type ConstCountMap = M.Map String Int
type LabelCountMap = M.Map String Int


type VarMap       = M.Map Label LocalVarMap
type LocalVarMap  = DM.DMap TypedIdent Value
type StrLitMap    = M.Map String SomeStrConst
type ClassMap     = DM.DMap SStr ClassInfo

type InheritanceMap = DM.DMap TypedIdent Reg
data ClassInfo t where
  ClassInfo ::
    { attrs       :: DList DS.Ident ts
    , attrTypes   :: DList DS.SLatteType ts
    , methods     :: [SomeFuncLabel]
    , vtableInfo  :: M.Map String FuncOrParent
    , constr      :: Maybe (Func 'Void '[Ptr (Struct t)])
    } -> ClassInfo t


data PotentialBlock = PotBlock 
  { blockLabel  :: Label
  , inherited   :: InheritanceMap
  , blockBody   :: [ComSimpleInstr]
  , branchInstr :: Maybe ComBranchInstr
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

    , owner       :: Maybe (DS.ClassIdent cls)
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
    , classMap      :: ClassMap

    , currentBlockLabel :: Maybe Label
    , currentFunc       :: Maybe PotentialFunc
    , currentProg       :: PotentialProg
    , currentScopeLevel :: Int

    , mallocTypes :: [Some SPrimType]
    , arrTypes    :: [Some SPrimType]   -- list of types of array elements
    } -> LLVMState

emptyState :: LLVMState
emptyState = LLVMState
  { regCounter = M.empty
  , constCounter = M.empty
  , labelCounter = M.empty

  , varMap    = M.empty
  , strLitMap = M.empty
  , classMap  = DM.empty

  , currentBlockLabel = Nothing
  , currentFunc       = Nothing
  , currentProg       = PotProg { mainFunc = Nothing, funcs = [] }
  , currentScopeLevel = 0
  
  , mallocTypes = []
  , arrTypes    = []
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

putClassMap :: LLVMConverter m => ClassMap -> m ()
putClassMap m = do
  LLVMState { classMap = _, .. } <- get
  put $ LLVMState { classMap = m, .. }

putMallocTypes :: LLVMConverter m => [Some SPrimType] -> m ()
putMallocTypes ts = do
  LLVMState { mallocTypes = _, .. } <- get
  put $ LLVMState { mallocTypes = ts, .. }

putArrTypes :: LLVMConverter m => [Some SPrimType] -> m ()
putArrTypes ts = do
  LLVMState { arrTypes = _, .. } <- get
  put $ LLVMState { arrTypes = ts, .. }

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

addCustomType :: LLVMConverter m => DS.ClassIdent cls -> m ()
addCustomType (DS.ClassIdent _ cls) = do
  m <- gets classMap
  
  let mbClsInfo = DM.lookup cls m
  unless (isNothing mbClsInfo) $ throwError internalClassAlredyDeclaredError

  let clsInfo = ClassInfo DNil DNil [] M.empty Nothing
  let newClsMap = DM.insert cls clsInfo m
  putClassMap newClsMap

putClassInfo :: LLVMConverter m
  => DS.ClassIdent cls -> ClassInfo cls -> m ()
putClassInfo (DS.ClassIdent _ cls) info = do
  m <- gets classMap
  let newClsMap = DM.insert cls info m
  putClassMap newClsMap

addAttr :: LLVMConverter m
  => DS.ClassIdent cls -> Sing t -> DS.Ident t -> m ()
addAttr (DS.ClassIdent _ cls) t attrId = do
  m <- gets classMap
  
  newClsInfo <- case DM.lookup cls m of
    Nothing ->
      pure $ ClassInfo (attrId :> DNil) (t :> DNil) [] M.empty Nothing

    Just ClassInfo { attrs = attrs, attrTypes = ts, .. } -> 
      pure $ ClassInfo { attrs = attrs <: attrId, attrTypes = ts <: t, .. }

  let newClsMap = DM.insert cls newClsInfo m
  putClassMap newClsMap

declareMethod :: LLVMConverter m
  => DS.ClassIdent cls -> Sing t -> Sing ts -> FuncLabel t ts -> m ()
declareMethod clsId@(DS.ClassIdent _ cls) retT argTs (FuncLabel f) = do
  ClassInfo { methods = methods, .. } <- getClassInfo cls
  
  let argTs' = SCons (SPtr $ SStruct cls) argTs
  let methods' = append' methods $ (retT, argTs') :&&: FuncLabel f
  putClassInfo clsId $ ClassInfo { methods = methods', .. }

  where
    append' :: [SomeFuncLabel] -> SomeFuncLabel -> [SomeFuncLabel]
    append' [] f = [f]
    append' (someF@(fT :&&: FuncLabel f) : fs) someG@(gT :&&: FuncLabel g)
      = if f == g then someG : fs else someF : append' fs someG

addMethod :: LLVMConverter m
  => DS.ClassIdent cls -> Sing t -> Sing ts -> Func t ts -> m ()
addMethod clsId@(DS.ClassIdent _ cls) retT argTs func = do
  ClassInfo { vtableInfo = vtable, .. } <- getClassInfo cls
  putClassInfo clsId
    $ ClassInfo 
      { vtableInfo
        = M.insert f ((argTs, retT) :&&: extractParam2' (Right func)) vtable
      , .. }

  where
    Func _ _ _ (FuncLabel f) _ = func
    



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

subScope :: LLVMConverter m => m a -> m a
subScope action = do
  incrScopeLevel
  result <- action
  decrScopeLevel
  return result


getBlockOrder :: LLVMConverter m => m [Label]
getBlockOrder = blockOrder <$> getCurrentFunc


getAttrNumber :: LLVMConverter m
  => SPrimType (Struct cls) -> DS.Ident t -> m Int
getAttrNumber (SStruct cls) attrId = do
  m <- gets classMap
  case DM.lookup cls m of
    Nothing -> throwError internalNoClassError
    Just ClassInfo { attrs = attrs, attrTypes = ts, .. } ->
      findAttr attrId attrs
    
  where
    findAttr :: LLVMConverter m => DS.Ident t -> DList DS.Ident ts -> m Int
    findAttr x DNil = throwError internalNoAttrError
    findAttr x (attr :> attrs)
      = if name attr == name x then return 1 else (1 +) <$> findAttr x attrs

getMethodNumber :: LLVMConverter m
  => SPrimType (Struct cls) -> DS.FuncIdent t ts -> m Int
getMethodNumber (SStruct cls) methodId = do
  m <- gets classMap
  case DM.lookup cls m of
    Nothing -> throwError internalNoClassError
    Just ClassInfo { methods = methods, .. } ->
      findMethod methodId methods
    
  where
    findMethod :: LLVMConverter m
      => DS.FuncIdent t ts -> [SomeFuncLabel] -> m Int
    findMethod _ [] = throwError internalNoMethodError
    findMethod f ((_ :&&: FuncLabel g) : funcs)

      = if g == name f then return 0 else (1 +) <$> findMethod f funcs


getClassInfo :: LLVMConverter m => SStr cls -> m (ClassInfo cls)
getClassInfo cls = do
  m <- gets classMap
  case DM.lookup cls m of
    Nothing -> throwError internalNoClassError
    Just info -> return info


resetCounters :: LLVMConverter m => m ()
resetCounters = putRegCounter M.empty >> putLabelCounter M.empty




