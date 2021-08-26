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


module LLVM.Cleanup where

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
import qualified Data.Some as Sm
import Data.GADT.Compare


import LLVM.LLVM
import LLVM.State
import LLVM.StateUtils
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition
import LLVM.Overwrite

import Dependent


-------------------------------------------------------------------------------
finishFunc :: LLVMConverter m => Pos -> m ()
finishFunc p = do
  dropZombieBlocks
  --dropAllRedundantInheritances
  fillInheritanceMaps
  addAllPhis
  
  PotFunc
    { label       = l
    , retType     = retT
    , argTypes    = argTs
    , args        = args
    , blockOrder  = order
    , .. } <- getCurrentFunc
  

  funcBody <- mapM (getBlock retT) order
  let func = Func (sGetPrimType retT) argTs args l funcBody
  
  addFunc p retT argTs func

  where
    getBlock retT label = do
      
      PotBlock 
        { blockLabel  = l
        , blockBody   = body
        , branchInstr = mbInstr
        , .. } <- getPotBlock label
          
      case mbInstr of
        Just instr ->
          return SimpleBlock { label = l, body = body, lastInstr = instr }
        Nothing -> case retT of
          DS.STVoid ->
            return SimpleBlock  { label = l
                                , body = body
                                , lastInstr = BrInstr RetVoid Nothing}
          
          _ -> throwError internalNoBranchError
          
          


addFunc :: LLVMConverter m
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




-------------------------------------------------------------------------------
fillInheritanceMaps :: LLVMConverter m => m ()
fillInheritanceMaps = do
  labels <- getBlockOrder
  loopThrough labels

  where
    loopThrough :: LLVMConverter m => [Label] -> m ()
    loopThrough labels = do
      changesMade <- foldl propagInheritances (pure False) labels
      when changesMade $ loopThrough labels
    
    propagInheritances :: LLVMConverter m => m Bool ->  Label -> m Bool
    propagInheritances acc label = do
      accChanges <- acc
      m <- getInheritanceMap label
      let inheritedIds = DM.keys m
      changesMade <- foldl (propagInheritance label) (pure False) inheritedIds
      return $ accChanges || changesMade
      
    propagInheritance :: LLVMConverter m
      => Label -> m Bool -> Sm.Some TypedIdent -> m Bool
    propagInheritance label acc (Sm.Some x) = do
      accChanges <- acc
      PotBlock { inputs = ins, .. } <- getPotBlock label
      changesMade <- any fst <$> mapM (getIdentValue' x) ins
      return $ accChanges || changesMade

addPhi :: LLVMConverter m => Label -> TypedIdent t -> m ()
addPhi label x@(TypedIdent singT _ lvl) = do
  m <- getInheritanceMap label
  let inheritedIds = DM.keys m
  unless (Sm.Some x `elem` inheritedIds)
    $ throwError internalPhiNotPartOfInheritedError

  block <- getPotBlock label
  when (isEntry block) $ throwError internalPhiInEntryError

  let ins = inputs block
  when (null ins) $ throwError internalNoInputsError

  reg <- getInherited label x
  vals <- mapM (getIdentValue x) ins
  prependInstr label
    $ Instr (Ass reg $ Phi singT (zip ins vals)) (Just $ show lvl)

addPhis :: LLVMConverter m => Label -> m ()
addPhis label = do
  m <- getInheritanceMap label
  let inheritedIds = DM.keys m
  mapM_ (addPhi' label) inheritedIds

  where
    addPhi' l (Sm.Some x) = addPhi l x


addAllPhis :: LLVMConverter m => m ()
addAllPhis = do
  labels <- getBlockOrder
  mapM_ addPhis labels



-------------------------------------------------------------------------------
dropZombieBlocks :: LLVMConverter m => m ()
dropZombieBlocks = do
  labels <- getBlockOrder
  changeMade <- or <$> mapM dropIfZombie labels
  when changeMade dropZombieBlocks

  where
  
    dropIfZombie :: LLVMConverter m => Label -> m Bool
    dropIfZombie label = do
      block <- getPotBlock label
      if isEntry block then
        return False
      else case inputs block of
        [] -> dropPotBlock label >> return True
        _ -> return False

    dropPotBlock :: LLVMConverter m => Label -> m ()
    dropPotBlock l = do
      labels <- getBlockOrder
      putBlockOrder $ delete l labels

      dropInputs l

      PotFunc { body = body, .. } <- getCurrentFunc
      putCurrentFunc $ PotFunc { body = M.delete l body, .. }
      

    dropInputs :: LLVMConverter m => Label -> m ()
    dropInputs l = do
      labels <- getBlockOrder
      mapM_ (dropInput l) labels

    dropInput :: LLVMConverter m => Label -> Label -> m ()
    dropInput input output = do
      PotBlock { inputs = ins, .. } <- getPotBlock output

      putPotBlock $ PotBlock { inputs = delete input ins, .. }




-------------------------------------------------------------------------------

-- TODO - does not work
dropAllRedundantInheritances :: LLVMConverter m => m ()
dropAllRedundantInheritances = do
  labels <- getBlockOrder
  mapM_ dropRedundantInheritances labels

dropRedundantInheritances :: LLVMConverter m => Label -> m ()
dropRedundantInheritances label = do
  PotBlock { inputs = ins, inherited = m, .. } <- getPotBlock label
  case ins of
    [] -> return ()
    (_ : _ : _) -> return ()
    [input] -> do
      let inheritedVars = DM.keys m
      mapM_ (dropRedundantInheritance input label) inheritedVars
    

  where
    dropRedundantInheritance input label (Sm.Some x@(TypedIdent singT _ _)) = do
      PotBlock { inherited = m, .. } <- getPotBlock label
      let currentReg = fromJust $ DM.lookup x m
      newVal <- getIdentValue x input

      overwriteReg singT currentReg newVal label
      putPotBlock $ PotBlock { inherited = DM.delete x m, .. }


overwriteReg :: LLVMConverter m => Sing t -> Reg t -> Value t -> Label -> m ()
overwriteReg singT reg val label = do
  PotBlock 
    { outputs = outs
    , blockBody = instrs
    , branchInstr = brInstr
    , .. } <- getPotBlock label

  putPotBlock
    $ PotBlock 
      { outputs = outs
      , blockBody = map (overwrite singT reg val) instrs
      , branchInstr = overwrite singT reg val <$> brInstr
      , .. }
  
  
    
  mapM_ (overwriteReg singT reg val) outs

  where

    overwriteVarMap :: LLVMConverter m
      => Sing t
      -> Reg t
      -> Value t
      -> Label
      -> m ()

    overwriteVarMap singT reg val label = do
      m <- getLocalVarMap label
      let inheritedVars = DM.keys m
      mapM_ (overwriteValue singT reg val label) inheritedVars


    overwriteValue :: LLVMConverter m
      => Sing t -> Reg t -> Value t -> Label -> Sm.Some TypedIdent -> m ()
    overwriteValue singT reg val label (Sm.Some x@(TypedIdent t _ _))
      = case gcompare singT t of
        GEQ -> do
          v <- getIdentValue x label
          let v' = if v == Var reg then val else v
          assignValue label x v'

        _ -> return ()
      
