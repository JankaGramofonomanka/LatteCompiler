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
import Data.List

import Data.Singletons.Sigma
import Data.Singletons.Prelude hiding ( Error )
import Data.Kind ( Type )
import qualified Data.Dependent.Map as DM

import LLVM.LLVM
import LLVM.State
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import Syntax.LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition
import qualified Constants as C

import Dependent
import SingChar



strConstLen :: String -> Some SNatural
strConstLen s = case toSing $ len s of
  SomeSing n -> Some $ SSucc n 
  
  where
    len :: String -> Natural
    len ""              = Zero
    len ('\\' : _ : s)  = Succ $ len s
    len (_ : s)         = Succ $ len s


self :: Value (Ptr (Struct s))
self = Var $ SpecialReg C.selfParam

mkVTableConst :: SStr s -> Constant (VTable s)
mkVTableConst cls = SpecialConst (C.mkVtableName $ singToString cls)

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

getNewLabelMid :: MonadState LLVMState m => m Label
getNewLabelMid = getNewLabel "Mid"

getIfElseLabels :: MonadState LLVMState m => m (Label, Label, Label)
getIfElseLabels = do
  labelIf   <- getNewLabel "If"
  labelElse <- getNewLabel "Else"
  labelJoin <- getNewLabel "IfJoin"
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
    Nothing   -> case strConstLen s of
      
      Some n -> do
        cst <- getNewConst C.strLitPrefix
        putStrLitMap $ M.insert s (n :&: cst) strMap
        return $ n :&: cst

getStrLitConstPtr :: MonadState LLVMState m => String -> m SomeStrConstPtr
getStrLitConstPtr s = do
  n :&: cst <- getStrLitConst s
  return $ n :&: ConstPtr cst


-------------------------------------------------------------------------------
addInstr :: LLVMConverter m => SimpleInstr -> m ()
addInstr instr = do
  l <- getCurrentBlockLabel
  PotBlock { branchInstr = mbInstr, .. } <- getPotBlock l

  -- ignore the instruction if it is after a branch instruction
  when (isNothing mbInstr) $ appendInstr l (Instr instr Nothing)

addComment :: LLVMConverter m => String -> m ()
addComment cmt = do
  l <- getCurrentBlockLabel
  appendInstr l $ Comment cmt

appendInstr :: LLVMConverter m => Label -> ComSimpleInstr -> m ()
appendInstr label instr = do
  PotBlock { blockBody = body, .. } <- getPotBlock label
  putPotBlock $ PotBlock { blockBody = body ++ [instr], .. }

prependInstr :: LLVMConverter m => Label -> ComSimpleInstr -> m ()
prependInstr label instr = do
  PotBlock { blockBody = body, .. } <- getPotBlock label
  putPotBlock $ PotBlock { blockBody = instr : body, .. }





addInput :: LLVMConverter m => Label -> Label -> m ()
addInput label newInput = do
  PotBlock { inputs = ins, .. } <- getPotBlock' label
  putPotBlock PotBlock { inputs = ins ++ [newInput], .. }

addOutput :: LLVMConverter m => Label -> Label -> m ()
addOutput label newOutput = do
  PotBlock { outputs = outs, .. } <- getPotBlock' label
  putPotBlock PotBlock { outputs = outs ++ [newOutput], .. }

addEdge :: LLVMConverter m => Label -> Label -> m ()
addEdge from to = addOutput from to >> addInput to from




addBranchInstr :: LLVMConverter m => Label -> BranchInstr -> m () -> m ()
addBranchInstr label instr preprocess = do
  PotBlock { branchInstr = mbInstr, .. } <- getPotBlock label
  case mbInstr of
    Nothing -> do
      preprocess
      putPotBlock $ PotBlock { branchInstr = Just (BrInstr instr Nothing), .. }
    
    Just (BrInstr Ret {} _) -> return ()
    Just (BrInstr RetVoid _) -> return ()
    Just (BrInstr ins _) -> throwError internalMultipleBranchesError  


branch :: LLVMConverter m => Label -> Label -> m ()
branch label output = do
  addBranchInstr label (Branch output) $ addEdge label output
    

condBranch :: LLVMConverter m => Label -> Value (I 1) -> Label -> Label -> m ()
condBranch label cond labelIf labelElse = do
  
  addBranchInstr label (CondBranch cond labelIf labelElse)
    $ addEdge label labelIf >> addEdge label labelElse

ret :: LLVMConverter m => Label -> Sing t -> Value t -> m ()
ret label singT value = addBranchInstr label (Ret singT value) $ return ()
  
retVoid :: LLVMConverter m => Label -> m ()
retVoid label = addBranchInstr label RetVoid $ return ()

branch' :: LLVMConverter m => Label -> m ()
branch' output = do
  l <- getCurrentBlockLabel
  branch l output

condBranch' :: LLVMConverter m => Value (I 1) -> Label -> Label -> m ()
condBranch' cond labelIf labelElse = do
  l <- getCurrentBlockLabel
  condBranch l cond labelIf labelElse

ret' :: LLVMConverter m => Sing t -> Value t -> m ()
ret' singT val = do
  l <- getCurrentBlockLabel
  ret l singT val

retVoid' :: LLVMConverter m => m ()
retVoid' = getCurrentBlockLabel >>= retVoid






-------------------------------------------------------------------------------
newMaybeEntryBlock :: LLVMConverter m => Bool -> Label -> m ()
newMaybeEntryBlock isEntry l = do
  PotBlock { isEntry = _, .. } <- getPotBlock' l
  putPotBlock $ PotBlock { isEntry = isEntry, .. }
  
  putCurrentBlockLabel l
  order <- getBlockOrder
  putBlockOrder $ order ++ [l]

newBlock :: LLVMConverter m => Label -> m ()
newBlock = newMaybeEntryBlock False

newEntryBlock  :: LLVMConverter m => Label -> m ()
newEntryBlock = newMaybeEntryBlock True



-------------------------------------------------------------------------------
getInherited' :: LLVMConverter m => Label -> TypedIdent t -> m (Bool, Reg t)
getInherited' l x = do

  PotBlock { inherited = m, .. } <- getPotBlock l
  case DM.lookup x m of
    Nothing -> do
      reg <- getNewReg (name x)
      putPotBlock $ PotBlock { inherited = DM.insert x reg m , .. }
      assignValue l x (Var reg)
      return (True, reg)

    Just reg -> return (False, reg)
  

getInherited :: LLVMConverter m => Label -> TypedIdent t -> m (Reg t)
getInherited l x = snd <$> getInherited' l x
  
getInheritanceMap :: LLVMConverter m => Label -> m InheritanceMap
getInheritanceMap l = do
  PotBlock { inherited = m, .. } <- getPotBlock l
  return m



getIdentValue' :: LLVMConverter m => TypedIdent t -> Label -> m (Bool, Value t)
getIdentValue' x l = do
  m <- getLocalVarMap l
  case DM.lookup x m of
    Just val  -> return (False, val)
    Nothing   -> do
      (changes, reg) <- getInherited' l x
      return (changes, Var reg)


getIdentValue :: LLVMConverter m => TypedIdent t -> Label -> m (Value t)
getIdentValue x l = snd <$> getIdentValue' x l
    

{-
assertRetTypeOK :: LLVMConverter m
  => BranchInstr -> m ()
assertRetTypeOK instr = case instr of
  Branch {}           -> return ()
  CondBranch {}       -> return ()
  Ret (t :&: v)       -> assertRetTypeIs t
  RetVoid             -> assertRetTypeIs SVoid

assertRetTypeIs ::LLVMConverter m
  => SPrimType t -> m ()
assertRetTypeIs t = throwTODO
-- -}

-------------------------------------------------------------------------------
assignValue :: LLVMConverter m => Label -> TypedIdent t -> Value t -> m ()
assignValue l x val = do
  m <- getLocalVarMap l      
  putLocalVarMap l $ DM.insert x val m



getDefaultValue :: LLVMConverter m => DS.TypeKW t -> m (Value (GetPrimType t))
getDefaultValue kw = case kw of

  DS.KWInt  _ -> return $ ILit 0
  DS.KWStr  _ -> do
    n :&: arrPtr <- getStrLitConstPtr ""
    zeroPtr <- getNewRegDefault

    addInstr $ Ass zeroPtr
      $ GetArrElemPtr (SArray i8 n) i32 i32 arrPtr (ILit 0) (ILit 0)
    return $ Var zeroPtr

  DS.KWBool _ -> return $ BoolLit False
  DS.KWVoid _ -> throwError $ voidDeclarationError (position kw)
  
  DS.KWArr t -> return Null
  DS.KWCustom clsId -> return Null



--------------------------------------------------------------------------------
addMallocType :: LLVMConverter m => SPrimType t -> m ()
addMallocType t = do
  ts <- gets mallocTypes
  unless (Some t `elem` ts) $ putMallocTypes $ ts ++ [Some t]

addArrType :: LLVMConverter m => SPrimType t -> m ()
addArrType elemT = do
  ts <- gets arrTypes
  unless (Some elemT `elem` ts) $ putArrTypes $ ts ++ [Some elemT]
  addMallocType elemT
