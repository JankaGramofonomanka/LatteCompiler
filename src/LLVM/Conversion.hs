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
module LLVM.Conversion where


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
import LLVM.State
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxGADTPosition



  
  


-------------------------------------------------------------------------------
true = BoolLit True
false = BoolLit False

getVarValue :: (MonadState LLVMState m, MonadError Error m)
  => S.Type t -> S.Var t -> m (Value (GetPrimType t))
getVarValue singT var = case var of
  S.Var     p id -> do
    let key = typedIdent singT id
    varMap <- gets varMap
    case DM.lookup key varMap of
      Nothing -> throwError $ noSuchVarError (position id) id
      Just reg -> return $ Var reg

  S.Member  {} -> throwTODO
  S.Elem    {} -> throwTODO
  S.Null    {} -> throwTODO
  S.Self    {} -> throwTODO



getExprValue :: (MonadState LLVMState m, MonadError Error m)
  => S.Type t -> S.Expr t -> m (Value (GetPrimType t))
getExprValue singT expr = case expr of
  S.EVar     p v -> getVarValue singT v

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
    v <- getExprValue singT e
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation MUL (ILit (-1)) v
    return $ Var reg
    
  S.Not p e -> do
    v <- getExprValue singT e
    reg <- getNewRegDefault

    cond <- getNewRegDefault
    labelIf <- getNewLabel
    labelElse <- getNewLabel
    labelJoin <- getNewLabel
    
    addInstr $ Ass cond $ ICMP EQ v true
    finishBlock (CondBranch (Var cond) labelIf labelElse)
    
    newBlock labelIf
    finishBlock $ Branch labelJoin
    newBlock labelElse
    finishBlock $ Branch labelJoin
    newBlock labelJoin

    addInstr $ Ass reg $ Phi [(labelIf, false), (labelElse, true)]
    return $ Var reg

  ---------------------------------------------------------------------
  S.EOp   p op e1 e2 -> do
    v1 <- getExprValue singT e1
    v2 <- getExprValue singT e2
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

getBinOp :: S.BinOp -> BinOp (I n)
getBinOp op = case op of
  S.Plus  p -> ADD
  S.Minus p -> SUB
  S.Times p -> MUL
  S.Div   p -> SDIV
  S.Mod   p -> SREM
