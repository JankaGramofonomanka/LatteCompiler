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
#-}
module LLVM.State where


import Prelude hiding (EQ)
import qualified Data.Map as M
import Data.Maybe
import Control.Monad.State
import Control.Monad.Except

import Data.Singletons.Sigma
import Data.Kind ( Type )

import LLVM.LLVM
import qualified Syntax.SyntaxGADT as S
import LangElemClasses
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

    , intRegMap     :: IntRegMap
    , boolRegMap    :: BoolRegMap
    , strRegMap     :: StrRegMap
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
      
      SomeList n -> do
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
  assertRetTypeOK instr
  let bl = SimpleBlock l body instr
  addBlock bl

addBlock :: MonadState LLVMState m => SimpleBlock -> m ()
addBlock bl = do
  PotFunc { body = blocks, .. } <- gets currentFunc
  putCurrentFunc $ PotFunc { body = blocks ++ [bl], .. }

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



-------------------------------------------------------------------------------
type family GetPrimType (t :: Type) :: PrimType where
  GetPrimType Int     = I 32
  GetPrimType Bool    = I 1
  GetPrimType String  = Ptr (I 8)
  GetPrimType S.Void  = Void

true = BoolLit True
false = BoolLit False

getVarValue :: (MonadState LLVMState m, MonadError Error m)
  => S.Var t -> m (Value (GetPrimType t))
getVarValue var = case var of
  S.Var     p id -> throwTODO

  S.Member  {} -> throwTODO
  S.Elem    {} -> throwTODO
  S.Null    {} -> throwTODO
  S.Self    {} -> throwTODO



-- {-
getExprValue :: (MonadState LLVMState m, MonadError Error m)
  => S.Expr t -> m (Value (GetPrimType t))
getExprValue expr = case expr of
  S.EVar     p v -> getVarValue v

  ---------------------------------------------------------------------
  S.ELitInt  p (S.SInt _ i) -> return (ILit i)
  S.ELitBool p b            -> return (BoolLit b)
  S.EString  p (S.SStr _ s) -> do
    (_ :&: ptr) <- getStrLitConstPtr s
    reg <- getNewRegDefault
    addInstr $ Ass reg $ GetElemPtr ptr (ILit 0)
    return $ Var reg

  ---------------------------------------------------------------------
  S.EApp p f args -> throwTODO
  
  ---------------------------------------------------------------------
  S.Neg p e -> do
    v <- getExprValue e
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation MUL (ILit (-1)) v
    return $ Var reg
    
  S.Not p e -> do
    v <- getExprValue e
    reg <- getNewRegDefault

    cond <- getNewRegDefault
    labelIf <- getNewLabel
    labelElse <- getNewLabel
    
    addInstr $ Ass cond $ ICMP EQ v true
    finishBlock (CondBranch (Var cond) labelIf labelElse)
    
    newBlock labelIf
    finishBlock $ Branch labelElse
    newBlock labelElse

    addInstr $ Ass reg $ Phi [(labelIf, false), (labelElse, true)]
    return $ Var reg

  ---------------------------------------------------------------------
  S.EOp   p op e1 e2 -> do
    v1 <- getExprValue e1
    v2 <- getExprValue e2
    let binOp = getBinOp op
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation binOp v1 v2
    return $ Var reg

  S.ERel  p op e1 e2 -> throwTODO
  S.EBool p op e1 e2 -> throwTODO
  
  ---------------------------------------------------------------------
  S.NewArr   p t e        -> throwTODO
  S.NewObj   p t          -> throwTODO
  S.Cast     p t e        -> throwTODO
  S.Concat   p e1 e2      -> throwTODO
-- -}

getBinOp :: S.BinOp -> BinOp (I n)
getBinOp op = case op of
  S.Plus  p -> ADD
  S.Minus p -> SUB
  S.Times p -> MUL
  S.Div   p -> SDIV
  S.Mod   p -> SREM
