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
import qualified Syntax.SyntaxDep as DS
import LLVM.State
import LLVM.TypeConversion
import LangElemClasses
import Errors
import Position.Position
import Position.SyntaxDepPosition



  
  


-------------------------------------------------------------------------------
true = BoolLit True
false = BoolLit False

getVarValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.SLatteType t -> DS.Var t -> m (Value (GetPrimType t))
getVarValue singT var = case var of
  DS.Var     p id -> do
    let key = typedIdent singT id
    varMap <- gets varMap
    case DM.lookup key varMap of
      Nothing -> throwError $ noSuchVarError (position id) id
      Just reg -> return $ Var reg

  DS.Attr {} -> throwTODO
  DS.Elem {} -> throwTODO
  DS.Null {} -> throwTODO
  DS.Self {} -> throwTODO



getExprValue :: (MonadState LLVMState m, MonadError Error m)
  => DS.SLatteType t -> DS.Expr t -> m (Value (GetPrimType t))
getExprValue singT expr = case expr of
  DS.EVar     p v -> getVarValue singT v

  ---------------------------------------------------------------------
  DS.ELitInt  p i -> return (ILit i)
  DS.ELitBool p b -> return (BoolLit b)
  DS.EString  p s -> do
    (_ :&: ptr) <- getStrLitConstPtr s
    reg <- getNewRegDefault
    addInstr $ Ass reg $ GetElemPtr ptr (ILit 0)
    return $ Var reg

  ---------------------------------------------------------------------
  DS.EApp p f args -> throwTODO
  
  ---------------------------------------------------------------------
  DS.Neg p e -> do
    v <- getExprValue singT e
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation MUL (ILit (-1)) v
    return $ Var reg
    
  DS.Not p e -> do
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
  DS.EOp   p op e1 e2 -> do
    v1 <- getExprValue singT e1
    v2 <- getExprValue singT e2
    let binOp = getBinOp op
    reg <- getNewRegDefault
    addInstr $ Ass reg $ BinOperation binOp v1 v2
    return $ Var reg

  DS.ERel  p op e1 e2 -> throwTODO
  DS.EBool p op e1 e2 -> throwTODO
  
  ---------------------------------------------------------------------
  DS.NewArr   p t e        -> throwTODO
  DS.NewObj   p t          -> throwTODO
  DS.Cast     p t e        -> throwTODO
  DS.Concat   p e1 e2      -> throwTODO

getBinOp :: DS.BinOp -> BinOp (I n)
getBinOp op = case op of
  DS.Plus  p -> ADD
  DS.Minus p -> SUB
  DS.Times p -> MUL
  DS.Div   p -> SDIV
  DS.Mod   p -> SREM
