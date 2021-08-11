{-# LANGUAGE 
    GADTs
  , StandaloneKindSignatures
  , DataKinds
  , TypeFamilies
  , PolyKinds
  , TypeOperators
  , RankNTypes
  , StandaloneDeriving
  , TypeApplications
  , TemplateHaskell
  , ScopedTypeVariables
  , FlexibleInstances
#-}

module LLVM.LLVM where

import Data.Kind ( Type )
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Sigma

import Dependent

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)


-- Primitive Types ------------------------------------------------------------
data PrimType where
  I     :: Nat -> PrimType
  Void  :: PrimType
  Ptr   :: PrimType -> PrimType
  Array :: PrimType -> Natural -> PrimType
  {-
    I'm using `Natural` instead of Nat because I can't figure out how to 
    create a list length of type `SNat`
  -}

type SPrimType :: PrimType -> Type
data SPrimType t where
  SI      :: SNat n -> SPrimType (I n)
  SVoid   :: SPrimType 'Void
  SPtr    :: SPrimType t -> SPrimType (Ptr t)
  SArray  :: SPrimType t -> SNatural n -> SPrimType (Array t n)

genDefunSymbols [''PrimType]

i1 :: SPrimType ('I 1)
i1 = sing @ (I 1)
i8 :: SPrimType ('I 8)
i8 = sing @ (I 8)
i32 :: SPrimType ('I 32)
i32 = sing @ (I 32)

type instance Sing = SPrimType

instance SingI n => SingI ('I n) where
  sing = SI sing

instance SingI 'Void where
  sing = SVoid

instance SingI t => SingI ('Ptr t) where
  sing = SPtr sing

instance (SingI n, SingI t) => SingI ('Array n t) where
  sing = SArray sing sing


deriving instance Show PrimType
deriving instance Eq PrimType
deriving instance Ord PrimType

deriving instance Show (SPrimType t)
deriving instance Eq (SPrimType t)
deriving instance Ord (SPrimType t)


-- Simple Values --------------------------------------------------------------
type Reg :: PrimType -> Type 
data Reg t where
  Reg :: String -> Int -> Reg t

type Constant :: PrimType -> Type
data Constant t where
  Const :: String -> Int -> Constant t

type Value :: PrimType -> Type
data Value t where
  Var       :: Reg t -> Value t
  ILit      :: Int -> Value (I n)
  BoolLit   :: Bool -> Value (I 1)
  ConstPtr  :: Constant t -> Value (Ptr t)


deriving instance Show (Reg t)
deriving instance Eq (Reg t)
deriving instance Ord (Reg t)

deriving instance Show (Constant t)
deriving instance Eq (Constant t)
deriving instance Ord (Constant t)

deriving instance Show (Value t)
deriving instance Eq (Value t)
deriving instance Ord (Value t)


-- Labels ---------------------------------------------------------------------
data Label = Label String Int deriving (Show, Eq, Ord)

type FuncLabel :: PrimType -> [PrimType] -> Type
data FuncLabel t ts where
  FuncLabel :: String -> FuncLabel t ts

  deriving Show


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
  deriving (Show, Eq, Ord)

deriving instance Show (BinOp t)
deriving instance Eq (BinOp t)
deriving instance Ord (BinOp t)

deriving instance Show (BitOp t)
deriving instance Eq (BitOp t)
deriving instance Ord (BitOp t)



-- Expression -----------------------------------------------------------------
type family ElemOf (t :: PrimType) :: PrimType where
  ElemOf (I n)      = I n
  ElemOf (Ptr t)    = Ptr t
  ElemOf (Array t n)  = t

type Expr :: PrimType -> Type
data Expr t where
  BinOperation  :: Sing t -> BinOp t -> Value t -> Value t -> Expr t
  Call          :: Sing t -> FuncLabel t ts -> ArgList ts -> Expr t
  GetElemPtr    :: Sing t -> Value (Ptr t)
                          -> Value (I n)
                          -> Expr (Ptr (ElemOf t))

  ICMP          :: Sing (I n) -> CMPKind
                              -> Value (I n)
                              -> Value (I n)
                              -> Expr (I 1)

  Phi           :: Sing t -> [(Label, Value t)] -> Expr t

deriving instance Show (Expr t)

-- Instructions ---------------------------------------------------------------
data SimpleInstr where
  Ass :: Reg t -> Expr t -> SimpleInstr
  VoidExpr :: Expr 'Void -> SimpleInstr

data BranchInstr where
  Branch :: Label -> BranchInstr
  CondBranch :: Value (I 1) -> Label -> Label -> BranchInstr
  Ret :: Sing t -> Value t -> BranchInstr
  RetVoid :: BranchInstr

deriving instance Show SimpleInstr
deriving instance Show BranchInstr

-- Simple Block ---------------------------------------------------------------
data SimpleBlock
  = SimpleBlock 
    { label :: Label
    , body :: [SimpleInstr]
    , lastInstr :: BranchInstr
    }
  
  deriving Show


-- Functions ------------------------------------------------------------------
type ArgList ts = DList Value ts
type ParamList ts = DList Reg ts

type Func :: PrimType -> [PrimType] -> Type
data Func t ts where
  Func :: SPrimType t
        -> ParamList ts
        -> FuncLabel t ts
        -> [SimpleBlock]
        -> Func t ts

deriving instance Show (DList Value ts)
deriving instance Show (DList Reg ts)
deriving instance Show (Func t ts)


-- Program --------------------------------------------------------------------
data LLVMProg
  = LLVM 
    { mainFunc :: Func (I 32) '[]
    , funcs :: [SomeFunc]
    , externFuncs :: [SomeFuncLabel]
    , constants :: [SomeConstant]
    }

-- Dependent Pairs ------------------------------------------------------------
type SomeReg        = Sigma PrimType (TyCon1 Reg)
type SomeConstant   = Sigma PrimType (TyCon1 Constant)
type SomeValue      = Sigma PrimType (TyCon1 Value)
type SomeBinOp      = Sigma PrimType (TyCon1 BinOp)
type SomeBitOp      = Sigma PrimType (TyCon1 BitOp)
type SomeExpr       = Sigma PrimType (TyCon1 Expr)
type SomeArgList    = Sigma [PrimType] (TyCon1 (DList Value))


type SomeFuncLabel  = Sigma2 PrimType [PrimType] (TyCon2 FuncLabel)
type SomeFunc       = Sigma2 PrimType [PrimType] (TyCon2 Func)


data StrConst :: Natural ~> Type
type instance Apply StrConst n = Constant (Array (I 8) n)
type SomeStrConst = Sigma Natural StrConst

data StrConstPtr :: Natural ~> Type
type instance Apply StrConstPtr n = (Value (Ptr (Array (I 8) n)))
type SomeStrConstPtr = Sigma Natural StrConstPtr

