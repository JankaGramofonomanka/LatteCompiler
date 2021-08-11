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
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM

import LLVM.LLVM
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent
import Control.Monad.RWS.Lazy (MonadState)

strLitPrefix :: [Char]
strLitPrefix = "str"

mkStrConst :: String -> String
mkStrConst s = s ++ "\00"


  
  

type RegCountMap = M.Map String Int
type ConstCountMap = M.Map String Int
type LabelCountMap = M.Map String Int
type BlockInfoMap = M.Map Label BlockInfo

type DeclarationPosMap = M.Map String Pos

type VarMap       = M.Map Label LocalVarMap
type LocalVarMap  = DM.DMap TypedIdent Value
type StrLitMap    = M.Map String SomeStrConst

data BlockInfo
  = BlockInfo
    { inputs :: [Label]
    , outputs :: [Label]
    }
    
emptyBlockInfo :: BlockInfo
emptyBlockInfo = BlockInfo [] []

type InheritenceMap = DM.DMap TypedIdent Reg

data PotentialBlock = PotBlock 
  { blockLabel :: Label
  --, inherited :: [InheritanceInfo]
  , inherited :: InheritenceMap
  , blockBody :: [SimpleInstr]
  }

data PotentialFunc where
  PotFunc ::
    { label :: FuncLabel t ts
    , retType :: Sing t
    , args :: ArgList ts
    --, body :: [SimpleBlock]
    --, body :: M.Map Label ([InheritanceInfo], SimpleBlock)
    , body :: M.Map Label (InheritenceMap, SimpleBlock)
    } -> PotentialFunc

data LLVMState where 
  LLVMState ::
    { regCounter    :: RegCountMap
    , constCounter  :: ConstCountMap
    , labelCounter  :: LabelCountMap

    , varMap        :: VarMap
    , strLitMap     :: StrLitMap
    , blockInfoMap  :: BlockInfoMap

    , declPosMap    :: DeclarationPosMap
    
    , currentBlock  :: Maybe PotentialBlock
    , currentFunc   :: PotentialFunc
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

putDeclPosMap :: MonadState LLVMState m => DeclarationPosMap -> m ()
putDeclPosMap m = do
  LLVMState { declPosMap = _, .. } <- get
  put $ LLVMState { declPosMap = m, .. }

putCurrentBlock :: MonadState LLVMState m => PotentialBlock -> m ()
putCurrentBlock b = do
  LLVMState { currentBlock = _, .. } <- get
  put $ LLVMState { currentBlock = Just b, .. }

putCurrentFunc :: MonadState LLVMState m => PotentialFunc -> m ()
putCurrentFunc f = do
  LLVMState { currentFunc = _, .. } <- get
  put $ LLVMState { currentFunc = f, .. }

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

-- getters --------------------------------------------------------------------
getNewReg :: MonadState LLVMState m => String -> m (Reg t)
getNewReg s = do
  counter <- gets regCounter
  let n = fromMaybe 0 $ M.lookup s counter
  putRegCounter $ M.insert s (n + 1) counter
  return $ Reg s n

getNewRegDefault :: MonadState LLVMState m => m (Reg t)
getNewRegDefault = getNewReg ""

getNewConst :: MonadState LLVMState m => String -> m (Constant t)
getNewConst s = do
  counter <- gets constCounter
  let n = fromMaybe 0 $ M.lookup s counter
  putConstCounter $ M.insert s (n + 1) counter
  return $ Const s n

getNewLabel :: MonadState LLVMState m => String -> m Label
getNewLabel s = do
  counter <- gets labelCounter
  let n = fromMaybe 0 $ M.lookup s counter
  putLabelCounter $ M.insert s (n + 1) counter
  return $ Label s n

getNewLabelDefault :: MonadState LLVMState m => m Label
getNewLabelDefault = getNewLabel ""

getIfElseLabels :: MonadState LLVMState m => m (Label, Label, Label)
getIfElseLabels = do
  labelIf   <- getNewLabel "If"
  labelElse <- getNewLabel "Else"
  labelJoin <- getNewLabel "IfElseJoin"
  return (labelIf, labelElse, labelJoin)

getWhileLabels :: MonadState LLVMState m => m (Label, Label, Label)
getWhileLabels = do
  labelCond <- getNewLabel "Cond"
  labelLoop <- getNewLabel "Loop"
  labelJoin <- getNewLabel "LoopJoin"
  return (labelCond, labelLoop, labelJoin)

getStrLitConst :: MonadState LLVMState m => String -> m SomeStrConst
getStrLitConst s = do
  strMap <- gets strLitMap
  case M.lookup s strMap of
    Just cst  -> return cst
    Nothing   -> case sListLength $ mkStrConst s of
      
      Some n -> do
        cst <- getNewConst strLitPrefix
        putStrLitMap $ M.insert s (n :&: cst) strMap
        return $ n :&: cst

getStrLitConstPtr :: MonadState LLVMState m => String -> m SomeStrConstPtr
getStrLitConstPtr s = do
  n :&: cst <- getStrLitConst s
  return $ n :&: ConstPtr cst

getBlockInfo :: (MonadState LLVMState m, MonadError Error m)
  => Label -> m BlockInfo
getBlockInfo l = do
  m <- gets blockInfoMap
  case M.lookup l m of
    Nothing -> throwError internalNoSuchBlockError
    Just info -> return info

getBlockInfo' :: (MonadState LLVMState m) => Label -> m BlockInfo
getBlockInfo' l = do
  m <- gets blockInfoMap
  case M.lookup l m of
    Nothing -> return emptyBlockInfo
    Just info -> return info


getLocalVarMap :: (MonadState LLVMState m) => Label -> m LocalVarMap
getLocalVarMap l = do
  m <- gets varMap
  case M.lookup l m of
    Just mm -> return mm
    Nothing -> putLocalVarMap l DM.empty >> return DM.empty

getCurrentVarMap :: MonadState LLVMState m => m LocalVarMap
getCurrentVarMap = getCurrentBlockLabel >>= getLocalVarMap

putCurrentVarMap :: MonadState LLVMState m => LocalVarMap -> m ()
putCurrentVarMap m = do
  l <- getCurrentBlockLabel
  putLocalVarMap l m
  
-------------------------------------------------------------------------------
addInstr :: MonadState LLVMState m => SimpleInstr -> m ()
addInstr instr = do
  PotBlock { blockBody = body, .. } <- getCurrentBlock
  putCurrentBlock $ PotBlock { blockBody = body ++ [instr], .. }

getCurrentBlock :: MonadState LLVMState m => m PotentialBlock
getCurrentBlock = do
  mbBlock <- gets currentBlock
  case mbBlock of
    Just bl -> return bl
    Nothing -> do
      l <- getNewLabelDefault
      let bl = PotBlock l DM.empty []
      putCurrentBlock bl
      return bl
    

newBlock :: MonadState LLVMState m => Label -> m ()
newBlock l = do
  info <- getBlockInfo' l
  putBlockInfo l info
  putCurrentBlock $ PotBlock l DM.empty []

finishBlock :: (MonadState LLVMState m, MonadError Error m)
  => BranchInstr -> m ()
finishBlock instr = do
  PotBlock
    { blockLabel = l
    , inherited = m
    , blockBody = body
    , .. } <- getCurrentBlock
  --assertRetTypeOK instr
  let bl = SimpleBlock l body instr
  addBlock m bl
  case instr of
    Branch ll -> addEdge l ll
    CondBranch _ ll1 ll2 ->
      addEdge l ll1 >> addEdge l ll2

    _ -> return ()

addBlock :: MonadState LLVMState m => InheritenceMap -> SimpleBlock -> m ()
addBlock m bl@SimpleBlock { label = l, .. } = do
  PotFunc { body = blocks, .. } <- gets currentFunc
  putCurrentFunc $ PotFunc { body = M.insert l (m, bl) blocks, .. }

getCurrentBlockLabel :: MonadState LLVMState m => m Label
getCurrentBlockLabel = do
  PotBlock { blockLabel = l, .. } <- getCurrentBlock
  return l

getCurrentBlockInfo :: (MonadState LLVMState m, MonadError Error m)
  => m BlockInfo
getCurrentBlockInfo = getCurrentBlockLabel >>= getBlockInfo

addBlockInfo :: (MonadState LLVMState m, MonadError Error m)
  => Label -> BlockInfo -> m ()
addBlockInfo l info = do
  m <- gets blockInfoMap
  case M.lookup l m of
    Nothing -> putBlockInfoMap $ M.insert l info m
    Just i -> throwError internalBlockAlredyExistsError


addInput :: MonadState LLVMState m => Label -> Label -> m ()
addInput block newInput = do
  BlockInfo { inputs = ins, .. } <- getBlockInfo' block
  putBlockInfo block $ BlockInfo { inputs = ins ++ [newInput], .. }

addOutput :: MonadState LLVMState m => Label -> Label -> m ()
addOutput block newOutput = do
  BlockInfo { outputs = outs, .. } <- getBlockInfo' block
  putBlockInfo block $ BlockInfo { outputs = outs ++ [newOutput], .. }

addEdge :: MonadState LLVMState m => Label -> Label -> m ()
addEdge from to = addOutput from to >> addInput to from

getInherited :: (MonadState LLVMState m, MonadError Error m)
  => Label -> Sing t -> DS.Ident t -> m (Reg (GetPrimType t))
getInherited l singT x = do
  currentL <- getCurrentBlockLabel
  let typedX = typedIdent singT x

  if l == currentL then do
    PotBlock { inherited = m, .. } <- getCurrentBlock
    case DM.lookup typedX m of
      Nothing -> do
        reg <- getNewReg (name x)
        putCurrentBlock $ PotBlock { inherited = DM.insert typedX reg m , .. }
        assignValue l singT x (Var reg)
        return reg

      Just reg -> return reg
  
  else do
    PotFunc { body = blocks, .. } <- gets currentFunc
    case M.lookup l blocks of
      Nothing -> throwError internalNoSuchBlockError
      
      Just (m, bl) -> case DM.lookup typedX m of
        Nothing -> do
          reg <- getNewReg (name x)
          let newBody = M.insert l (DM.insert typedX reg m, bl) blocks
          putCurrentFunc $ PotFunc { body = newBody, .. }
          assignValue l singT x (Var reg)
          return reg
        
        Just reg -> return reg

{-
assertRetTypeOK :: (MonadState LLVMState m, MonadError Error m)
  => BranchInstr -> m ()
assertRetTypeOK instr = case instr of
  Branch {}           -> return ()
  CondBranch {}       -> return ()
  Ret (t :&: v)       -> assertRetTypeIs t
  RetVoid             -> assertRetTypeIs SVoid

assertRetTypeIs ::(MonadState LLVMState m, MonadError Error m)
  => SPrimType t -> m ()
assertRetTypeIs t = throwTODO
-- -}

-------------------------------------------------------------------------------
assignValue :: (MonadState LLVMState m, MonadError Error m)
  => Label -> Sing t -> DS.Ident t -> Value (GetPrimType t) -> m ()
assignValue l singT x val = do
  let key = typedIdent singT x
  m <- getLocalVarMap l      
  putLocalVarMap l $ DM.insert key val m



getDeclPos ::
  ( MonadState LLVMState m
  , IsIdent i
  , MonadError Error m
  , HasPosition i
  )
  => i -> m Pos
getDeclPos x = do
  m <- gets declPosMap
  case M.lookup (name x) m of
    Nothing -> throwError $ noSuchVarError (position x) x
    Just p -> return p

getDefaultValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.TypeKW t -> m (Value (GetPrimType t))
getDefaultValue kw = case kw of

  DS.KWInt  _ -> return $ ILit 0
  DS.KWStr  _ -> do
    _ :&: arrPtr <- getStrLitConstPtr ""
    zeroPtr <- getNewRegDefault
    addInstr $ Ass zeroPtr $ GetElemPtr arrPtr (ILit 0)
    return $ Var zeroPtr

  DS.KWBool _ -> return $ BoolLit False
  DS.KWVoid _ -> throwError $ voidDeclarationError (position kw)
  
  DS.KWArr t -> Var <$> getNewRegDefault
  DS.KWCustom clsId -> Var <$> getNewRegDefault

    
      
    



