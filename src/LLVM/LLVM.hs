{-# LANGUAGE 
    GADTs,
    StandaloneKindSignatures,
    DataKinds,
    TypeFamilies,
    PolyKinds,
    TypeOperators
#-}

module LLVM.LLVM where

import Data.Kind ( Type )
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude

type DPair :: (t -> Type) -> Type
data DPair a where
  DPair :: { fst :: Sing t, snd :: a t } -> DPair a

-- Primitive Types ------------------------------------------------------------
data PrimType where
  I     :: Nat -> PrimType
  Void  :: PrimType
  Ptr   :: PrimType -> PrimType
  Arr   :: Nat -> PrimType -> PrimType

type SPrimType :: PrimType -> Type
data SPrimType t where
  SI    :: SNat n -> SPrimType (I n)
  SVoid :: SPrimType 'Void
  SPtr  :: SPrimType t -> SPrimType (Ptr t)
  SArr  :: SNat n -> SPrimType t -> SPrimType (Arr n t)

type instance Sing = SPrimType

instance SingI n => SingI ('I n) where
  sing = SI sing

instance SingI 'Void where
  sing = SVoid

instance SingI t => SingI ('Ptr t) where
  sing = SPtr sing

instance (SingI n, SingI t) => SingI ('Arr n t) where
  sing = SArr sing sing

-- Simple Values --------------------------------------------------------------
type Reg :: PrimType -> Type 
data Reg t where
  Reg :: String -> Reg t

type Constant :: PrimType -> Type
data Constant t where
  Const :: String -> Constant t

type Value :: PrimType -> Type
data Value t where
  Var :: Reg t -> Value t
  ILit :: Int -> Value (I n)
  ConstPtr :: Constant t -> Value (Ptr t)

newtype Label = Label Int

-- Functions ------------------------------------------------------------------
type FuncLabel :: PrimType -> [PrimType] -> Type
data FuncLabel t ts where
  FuncLabel :: String -> FuncLabel t ts

  deriving Show


type ArgList :: [PrimType] -> Type
data ArgList ts where
  Nil :: ArgList '[]
  (:>) :: Value t -> ArgList ts -> ArgList (t : ts)
infixr 5 :>

type Func :: PrimType -> [PrimType] -> Type
data Func t ts where
  Func :: SPrimType t
        -> ArgList ts
        -> FuncLabel t ts
        -> [SimpleBlock]
        -> Func t ts


-- Operators ------------------------------------------------------------------
type BinOp :: PrimType -> Type
data BinOp t where
  ADD   :: BinOp (I n)
  SUB   :: BinOp (I n)
  MUL   :: BinOp (I n)
  SDIV  :: BinOp (I n)
  UDIV  :: BinOp (I n)
  SREM  :: BinOp (I n)
  UREM  :: BinOp (I n)

type BitOp :: PrimType -> Type
data BitOp t where
  SHL   :: BitOp (I n)
  LSHR  :: BitOp (I n)
  ASHR  :: BitOp (I n)
  AND   :: BitOp (I n)
  OR    :: BitOp (I n)
  XOR   :: BitOp (I n)


data CMPKind = EQ | NE | SGT | SGE | SLT | SLE | UGT | UGE | ULT | ULE

-- Expression -----------------------------------------------------------------
type Expr :: PrimType -> Type
data Expr t where
  BinOperation :: BinOp t -> Value t -> Value t -> Expr t
  Call :: FuncLabel t ts -> ArgList ts -> Expr t
  GetElemPtr :: Value (Ptr t) -> Value (I n) -> Expr (Ptr t)
  ICMP :: CMPKind -> Value (I n) -> Value (I n) -> Expr (I 1)
  Phi :: [(Label, Value t)] -> Expr t


-- Instructions ---------------------------------------------------------------
data SimpleInstr where
  Ass :: Reg t -> Expr t -> SimpleInstr
  VoidExpr :: Expr 'Void -> SimpleInstr

data BranchInstr where
  Branch :: Label -> BranchInstr
  CondBranch :: Value (I 1) -> Label -> Label -> BranchInstr
  Ret :: Value t -> BranchInstr
  RetVoid :: BranchInstr

-- Simple Block ---------------------------------------------------------------
data SimpleBlock
  = SimpleBlock {
      label :: Label,
      body :: [SimpleInstr],
      lastInstr :: BranchInstr
    }












