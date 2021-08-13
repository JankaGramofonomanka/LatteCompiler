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

module LLVM.StateUtils where


import Prelude hiding ( EQ )
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error )
import Data.Singletons.TypeLits hiding ( Error )
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM
import qualified Data.Some as D

import LLVM.LLVM
import LLVM.State
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent

strLitPrefix :: [Char]
strLitPrefix = "str"

mkStrConst :: String -> String
mkStrConst s = s ++ "\00"


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
    Nothing -> do
      scL <- gets currentScopeLevel
      return $ emptyBlockInfo scL

    Just info -> return info

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

getCurrentFunc :: (MonadState LLVMState m, MonadError Error m)
  => m PotentialFunc
getCurrentFunc = do
  mbFunc <- gets currentFunc
  case mbFunc of
    Just bl -> return bl
    Nothing -> throwError internalNoFuncError

getCurrentBlockLabel :: MonadState LLVMState m => m Label
getCurrentBlockLabel = do
  PotBlock { blockLabel = l, .. } <- getCurrentBlock
  return l

getCurrentBlockInfo :: (MonadState LLVMState m, MonadError Error m)
  => m BlockInfo
getCurrentBlockInfo = getCurrentBlockLabel >>= getBlockInfo

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
  
incrScopeLevel :: MonadState LLVMState m => m ()
incrScopeLevel = do
  LLVMState { currentScopeLevel = l, .. } <- get
  put $ LLVMState { currentScopeLevel = l + 1, .. }

decrScopeLevel :: (MonadState LLVMState m, MonadError Error m) => m ()
decrScopeLevel = do
  LLVMState { currentScopeLevel = l, .. } <- get
  when (l <= 0) $ throwError internalScopeLevelBelowZeroError
  put $ LLVMState { currentScopeLevel = l - 1, .. }


-------------------------------------------------------------------------------
addInstr :: MonadState LLVMState m => SimpleInstr -> m ()
addInstr instr = do
  PotBlock { blockBody = body, .. } <- getCurrentBlock
  putCurrentBlock $ PotBlock { blockBody = body ++ [instr], .. }


    

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

addBlock :: (MonadState LLVMState m, MonadError Error m)
  => InheritanceMap -> SimpleBlock -> m ()
addBlock m bl@SimpleBlock { label = l, .. } = do
  PotFunc { body = blocks, .. } <- getCurrentFunc
  putCurrentFunc $ PotFunc { body = M.insert l (m, bl) blocks, .. }

finishFunc :: (MonadState LLVMState m, MonadError Error m)
  => Pos -> m ()
finishFunc p = do
  fillInheritanceMaps
  addAllPhis

  PotFunc
    { label = l
    , retType = retT
    , argTypes = argTs
    , args = args
    , body = body
    , .. } <- getCurrentFunc
  
  let funcBody = map (snd . snd) $ M.toList body
  let func = Func (sGetPrimType retT) argTs args l funcBody
  
  addFunc p retT argTs func

addFunc :: (MonadState LLVMState m, MonadError Error m)
  => Pos -> DS.SLatteType t -> Sing ts -> Func (GetPrimType t) ts -> m ()
addFunc p t singTs func@(Func singT _ _ (FuncLabel funcName) _) = do
  if funcName == "main" then case t of
    DS.STInt -> case singTs of
      SCons _ _ -> throwError $ mainWithArgsError p
      SNil -> do
        PotProg { mainFunc = _, .. } <- gets currentProg
        putCurrentProg
          $ PotProg { mainFunc = Just func, .. }    
      
    _ -> throwError $ mainNotIntError p
  
  else do
    PotProg { funcs = funcs, .. } <- gets currentProg
    putCurrentProg
      $ PotProg { funcs = funcs ++ [(singT, singTs) :&&: func], .. }






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

getInherited' :: (MonadState LLVMState m, MonadError Error m)
  => Label -> TypedIdent t -> m (Bool, Reg t)
getInherited' l x = do
  currentL <- getCurrentBlockLabel

  if l == currentL then do
    PotBlock { inherited = m, .. } <- getCurrentBlock
    case DM.lookup x m of
      Nothing -> do
        reg <- getNewReg (name x)
        putCurrentBlock $ PotBlock { inherited = DM.insert x reg m , .. }
        assignValue l x (Var reg)
        return (True, reg)

      Just reg -> return (False, reg)
  
  else do
    PotFunc { body = blocks, .. } <- getCurrentFunc
    case M.lookup l blocks of
      Nothing -> throwError internalNoSuchBlockError
      
      Just (m, bl) -> case DM.lookup x m of
        Nothing -> do
          reg <- getNewReg (name x)
          let newBody = M.insert l (DM.insert x reg m, bl) blocks
          putCurrentFunc $ PotFunc { body = newBody, .. }
          assignValue l x (Var reg)
          return (True, reg)
        
        Just reg -> return (False, reg)


getInherited :: (MonadState LLVMState m, MonadError Error m)
  => Label -> TypedIdent t -> m (Reg t)
getInherited l x = snd <$> getInherited' l x
  
getInheritanceMap :: (MonadState LLVMState m, MonadError Error m)
  => Label -> m InheritanceMap
getInheritanceMap l = do
  currentL <- getCurrentBlockLabel

  if l == currentL then do
    PotBlock { inherited = m, .. } <- getCurrentBlock
    return m
  
  else do
    PotFunc { body = blocks, .. } <- getCurrentFunc
    case M.lookup l blocks of
      Nothing -> throwError internalNoSuchBlockError
      Just (m, bl) -> return m



getIdentValue :: (MonadState LLVMState m, MonadError Error m)
  => TypedIdent t -> Label -> m (Value t)
getIdentValue x l = do
  m <- getLocalVarMap l
  case DM.lookup x m of
    Just val  -> return val
    Nothing   -> Var <$> getInherited l x
    

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
  => Label -> TypedIdent t -> Value t -> m ()
assignValue l x val = do
  m <- getLocalVarMap l      
  putLocalVarMap l $ DM.insert x val m



getDefaultValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.TypeKW t -> m (Value (GetPrimType t))
getDefaultValue kw = case kw of

  DS.KWInt  _ -> return $ ILit 0
  DS.KWStr  _ -> do
    n :&: arrPtr <- getStrLitConstPtr ""
    zeroPtr <- getNewRegDefault

    addInstr $ Ass zeroPtr $ GetElemPtr (SArray i8 n) i32 arrPtr (ILit 0)
    return $ Var zeroPtr

  DS.KWBool _ -> return $ BoolLit False
  DS.KWVoid _ -> throwError $ voidDeclarationError (position kw)
  
  DS.KWArr t -> Var <$> getNewRegDefault
  DS.KWCustom clsId -> Var <$> getNewRegDefault

fillInheritanceMaps :: (MonadState LLVMState m, MonadError Error m) => m ()
fillInheritanceMaps = do
  labels <- gets (map fst . M.toList . blockInfoMap)
  loopThrough labels

  where
    loopThrough :: (MonadState LLVMState m, MonadError Error m)
      => [Label] -> m ()
    loopThrough labels = do
      changesMade <- foldl propagInheritances (pure False) labels
      when changesMade $ loopThrough labels
    
    propagInheritances :: (MonadState LLVMState m, MonadError Error m)
      => m Bool ->  Label -> m Bool
    propagInheritances acc label = do
      accChanges <- acc
      m <- getInheritanceMap label
      let inheritedIds = DM.keys m
      changesMade <- foldl (propagInheritance label) (pure False) inheritedIds
      return $ accChanges || changesMade
      
    propagInheritance :: (MonadState LLVMState m, MonadError Error m)
      => Label -> m Bool -> D.Some TypedIdent -> m Bool
    propagInheritance label acc (D.Some x) = do
      accChanges <- acc
      BlockInfo { inputs = ins, .. } <- getBlockInfo label
      changesMade <- any fst <$> mapM (\l -> getInherited' l x) ins
      return $ accChanges || changesMade


addPhi :: (MonadState LLVMState m, MonadError Error m)
  => Label -> TypedIdent t -> m ()
addPhi label x@(TypedIdent singT _) = do
  m <- getInheritanceMap label
  let inheritedIds = DM.keys m
  unless (D.Some x `elem` inheritedIds)
    $ throwError internalPhiNotPartOfInheritedError

  BlockInfo { inputs = ins, .. } <- getBlockInfo label
  reg <- getInherited label x
  vals <- mapM (getIdentValue x) ins
  addInstr $ Ass reg $ Phi singT (zip ins vals)

addPhis :: (MonadState LLVMState m, MonadError Error m)
  => Label -> m ()
addPhis label = do
  m <- getInheritanceMap label
  let inheritedIds = DM.keys m
  mapM_ (addPhi' label) inheritedIds

  where
    addPhi' l (D.Some x) = addPhi l x


addAllPhis :: (MonadState LLVMState m, MonadError Error m) => m ()
addAllPhis = do
  labels <- gets (map fst . M.toList . blockInfoMap)
  mapM_ addPhis labels

