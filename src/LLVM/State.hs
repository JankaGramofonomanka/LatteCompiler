{-# LANGUAGE
    RecordWildCards
  , FlexibleContexts
  
  , DataKinds
  , ScopedTypeVariables
  , KindSignatures
  , TypeApplications
  , TypeOperators
#-}
module LLVM.State where


import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Singletons.TypeLits
import Data.Singletons.Prelude

import LLVM.LLVM
import qualified Syntax.SyntaxGADT as S
import Errors

strLitPrefix :: [Char]
strLitPrefix = "str"

mkStrConst :: String -> String
mkStrConst s = s ++ "\00"

type RegCountMap = M.Map String Int
type ConstCountMap = M.Map String Int

type IntRegMap  = M.Map (S.Ident Int) (Reg (I 32))
type BoolRegMap = M.Map (S.Ident Bool) (Reg (I 1))
type StrRegMap  = M.Map (S.Ident String) (Reg (Ptr (I 8)))

type StrLitMap  = M.Map String SomeStrConst

data LLVMState = LLVMState
  { regCounter    :: RegCountMap
  , constCounter  :: ConstCountMap
  , intRegMap     :: IntRegMap
  , boolRegMap    :: BoolRegMap
  , strRegMap     :: StrRegMap
  , strLitMap     :: StrLitMap
  }


-- putters --------------------------------------------------------------------
putRegCounter :: MonadState LLVMState m => RegCountMap -> m ()
putRegCounter m = do
  LLVMState { regCounter = _, .. } <- get
  put $ LLVMState { regCounter = m, .. }

putConstCounter :: MonadState LLVMState m => ConstCountMap -> m ()
putConstCounter m = do
  LLVMState { constCounter = _, .. } <- get
  put $ LLVMState { constCounter = m, .. }

putIntRegMap :: MonadState LLVMState m => IntRegMap -> m ()
putIntRegMap m = do
  LLVMState { intRegMap = _, .. } <- get
  put $ LLVMState { intRegMap = m, .. }

putBoolRegMap :: MonadState LLVMState m => BoolRegMap -> m ()
putBoolRegMap m = do
  LLVMState { boolRegMap = _, .. } <- get
  put $ LLVMState { boolRegMap = m, .. }

putStrRegMap :: MonadState LLVMState m => StrRegMap -> m ()
putStrRegMap m = do
  LLVMState { strRegMap = _, .. } <- get
  put $ LLVMState { strRegMap = m, .. }

putStrLitMap :: MonadState LLVMState m => StrLitMap -> m ()
putStrLitMap m = do
  LLVMState { strLitMap = _, .. } <- get
  put $ LLVMState { strLitMap = m, .. }



-- getters --------------------------------------------------------------------
getNewReg :: MonadState LLVMState m => String -> m (Reg t)
getNewReg s = do
  counter <- gets regCounter
  let n = fromMaybe 0 $ M.lookup s counter
  putRegCounter $ M.insert s (n + 1) counter
  return $ Reg s n

getNewConst :: MonadState LLVMState m => String -> m (Constant t)
getNewConst s = do
  counter <- gets constCounter
  let n = fromMaybe 0 $ M.lookup s counter
  putRegCounter $ M.insert s (n + 1) counter
  return $ Const s n



getStrLitConst :: MonadState LLVMState m => String -> m SomeStrConst
getStrLitConst s = do
  strMap <- gets strLitMap
  case M.lookup s strMap of
    Just cst  -> return cst
    Nothing   -> case sListLength $ mkStrConst s of
      
      SomeList n -> do
        cst <- getNewConst strLitPrefix
        putStrLitMap $ M.insert s (n :&: cst) strMap
        return $ n :&: cst

getStrLitConstPtr :: MonadState LLVMState m => String -> m SomeStrConstPtr
getStrLitConstPtr s = do
  n :&: cst <- getStrLitConst s
  return $ n :&: ConstPtr cst
  
  

  


