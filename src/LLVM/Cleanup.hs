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
import qualified Data.Some as D

import LLVM.LLVM
import LLVM.State
import LLVM.StateUtils
import qualified Syntax.SyntaxDep as DS
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition

import Dependent


-------------------------------------------------------------------------------
finishFunc :: LLVMConverter m => Pos -> m ()
finishFunc p = do
  dropZombieBlocks
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
        , inputs = ins
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
      => Label -> m Bool -> D.Some TypedIdent -> m Bool
    propagInheritance label acc (D.Some x) = do
      accChanges <- acc
      PotBlock { inputs = ins, .. } <- getPotBlock label
      changesMade <- any fst <$> mapM (getIdentValue' x) ins
      return $ accChanges || changesMade

addPhi :: LLVMConverter m => Label -> TypedIdent t -> m ()
addPhi label x@(TypedIdent singT _ lvl) = do
  m <- getInheritanceMap label
  let inheritedIds = DM.keys m
  unless (D.Some x `elem` inheritedIds)
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
    addPhi' l (D.Some x) = addPhi l x


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








