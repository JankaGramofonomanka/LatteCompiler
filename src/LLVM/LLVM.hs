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
#-}

module LLVM.LLVM where

import Data.Kind ( Type )
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Sigma

data Natural = Zero | Succ Natural deriving (Show, Eq, Ord)
genSingletons [''Natural]
deriving instance Show (SNatural n)
deriving instance Eq (SNatural n)
deriving instance Ord (SNatural n)

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)


-- Primitive Types ------------------------------------------------------------
data PrimType where
  I     :: Nat -> PrimType
  Void  :: PrimType
  Ptr   :: PrimType -> PrimType
  Arr   :: PrimType -> Natural -> PrimType
  {-| I'm using `Natural` instead of Nat because I can't figure out how to 
      create a list length of type `SNat`
  -}

type SPrimType :: PrimType -> Type
data SPrimType t where
  SI    :: SNat n -> SPrimType (I n)
  SVoid :: SPrimType 'Void
  SPtr  :: SPrimType t -> SPrimType (Ptr t)
  SArr  :: SPrimType t -> SNatural n -> SPrimType (Arr t n)



type instance Sing = SPrimType

instance SingI n => SingI ('I n) where
  sing = SI sing

instance SingI 'Void where
  sing = SVoid

instance SingI t => SingI ('Ptr t) where
  sing = SPtr sing

instance (SingI n, SingI t) => SingI ('Arr n t) where
  sing = SArr sing sing


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
newtype Label = Label Int deriving (Show, Eq, Ord)

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
  ElemOf (Arr t n)  = t

type Expr :: PrimType -> Type
data Expr t where
  BinOperation :: BinOp t -> Value t -> Value t -> Expr t
  Call :: FuncLabel t ts -> ArgList ts -> Expr t
  GetElemPtr :: Value (Ptr t) -> Value (I n) -> Expr (Ptr (ElemOf t))
  ICMP :: CMPKind -> Value (I n) -> Value (I n) -> Expr (I 1)
  Phi :: [(Label, Value t)] -> Expr t

deriving instance Show (Expr t)

-- Instructions ---------------------------------------------------------------
data SimpleInstr where
  Ass :: Reg t -> Expr t -> SimpleInstr
  VoidExpr :: Expr 'Void -> SimpleInstr

data BranchInstr where
  Branch :: Label -> BranchInstr
  CondBranch :: Value (I 1) -> Label -> Label -> BranchInstr
  Ret :: SomeValue -> BranchInstr
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
type ArgList :: [PrimType] -> Type
data ArgList ts where
  Nil   :: ArgList '[]
  (:>)  :: Value t -> ArgList ts -> ArgList (t : ts)
infixr 5 :>

type Func :: PrimType -> [PrimType] -> Type
data Func t ts where
  Func :: SPrimType t
        -> ArgList ts
        -> FuncLabel t ts
        -> [SimpleBlock]
        -> Func t ts

deriving instance Show (ArgList ts)
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
type SomeArgList    = Sigma [PrimType] (TyCon1 ArgList)

data Sigma2 (t1 :: Type) (t2 :: Type) :: (t1 ~> t2 ~> *) -> * where
  (:&&:) 
    :: (Sing (v1 :: t1), Sing (v2 :: t2))
    -> f @@ v1 @@ v2 -> Sigma2 t1 t2 f
infixr 5 :&&:
type SomeFuncLabel  = Sigma2 PrimType [PrimType] (TyCon2 FuncLabel)
type SomeFunc       = Sigma2 PrimType [PrimType] (TyCon2 Func)


data StrConst :: Natural ~> Type
type instance Apply StrConst n = Constant (Arr (I 8) n)
type SomeStrConst = Sigma Natural StrConst

data StrConstPtr :: Natural ~> Type
type instance Apply StrConstPtr n = (Value (Ptr (Arr (I 8) n)))
type SomeStrConstPtr = Sigma Natural StrConstPtr


type Some :: (k -> Type) -> Type
data Some f where
  Some :: SingI v => f v -> Some f

data SomeList t where
  SomeList :: SList (l :: [t]) -> SomeList t



-- Some Utils -----------------------------------------------------------------
listLength :: [a] -> Natural
listLength [] = Zero
listLength (x : xs) = Succ $ listLength xs

sListLength :: [a] -> Some SNatural
sListLength [] = Some SZero
sListLength (c : cs) = case sListLength cs of
  Some n -> Some $ SSucc n



