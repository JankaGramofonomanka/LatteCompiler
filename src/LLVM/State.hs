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
import qualified Syntax.SyntaxGADT as S
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxGADTPosition

strLitPrefix :: [Char]
strLitPrefix = "str"

mkStrConst :: String -> String
mkStrConst s = s ++ "\00"


  
  

type RegCountMap = M.Map String Int
type ConstCountMap = M.Map String Int

type VarMap     = DM.DMap TypedIdent Reg
type TypeMap    = DM.DMap S.Ident S.Type
type StrLitMap  = M.Map String SomeStrConst

data PotentialBlock = PotBlock Label [SimpleInstr]
data PotentialFunc where
  PotFunc ::
    { label :: FuncLabel t ts
    , retType :: Sing t
    , args :: ArgList ts
    , body :: [SimpleBlock]
    } -> PotentialFunc

data LLVMState where 
  LLVMState ::
    { regCounter    :: RegCountMap
    , constCounter  :: ConstCountMap
    , labelCounter  :: Int

    , varMap        :: VarMap
    , typeMap       :: TypeMap
    , strLitMap     :: StrLitMap
    
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

putLabelCounter :: MonadState LLVMState m => Int -> m ()
putLabelCounter n = do
  LLVMState { labelCounter = _, .. } <- get
  put $ LLVMState { labelCounter = n, .. }

putVarMap :: MonadState LLVMState m => VarMap -> m ()
putVarMap m = do
  LLVMState { varMap = _, .. } <- get
  put $ LLVMState { varMap = m, .. }

putTypeMap :: MonadState LLVMState m => TypeMap -> m ()
putTypeMap m = do
  LLVMState { typeMap = _, .. } <- get
  put $ LLVMState { typeMap = m, .. }

putStrLitMap :: MonadState LLVMState m => StrLitMap -> m ()
putStrLitMap m = do
  LLVMState { strLitMap = _, .. } <- get
  put $ LLVMState { strLitMap = m, .. }

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
  putRegCounter $ M.insert s (n + 1) counter
  return $ Const s n

getNewLabel :: MonadState LLVMState m => m Label
getNewLabel = do
  n <- gets labelCounter
  putLabelCounter (n + 1)
  return $ Label n

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
  


  
-------------------------------------------------------------------------------
addInstr :: MonadState LLVMState m => SimpleInstr -> m ()
addInstr instr = do
  PotBlock l body <- getCurrentBlock
  putCurrentBlock $ PotBlock l (body ++ [instr])

getCurrentBlock :: MonadState LLVMState m => m PotentialBlock
getCurrentBlock = do
  mbBlock <- gets currentBlock
  case mbBlock of
    Just bl -> return bl
    Nothing -> do
      l <- getNewLabel
      let bl = PotBlock l []
      putCurrentBlock bl
      return bl
    

newBlock :: MonadState LLVMState m => Label -> m ()
newBlock l = putCurrentBlock $ PotBlock l []

finishBlock :: (MonadState LLVMState m, MonadError Error m)
  => BranchInstr -> m ()
finishBlock instr = do
  PotBlock l body <- getCurrentBlock
  --assertRetTypeOK instr
  let bl = SimpleBlock l body instr
  addBlock bl

addBlock :: MonadState LLVMState m => SimpleBlock -> m ()
addBlock bl = do
  PotFunc { body = blocks, .. } <- gets currentFunc
  putCurrentFunc $ PotFunc { body = blocks ++ [bl], .. }

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


