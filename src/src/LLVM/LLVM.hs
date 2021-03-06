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

import qualified Data.Map as M
import Data.Kind ( Type )
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Data.Singletons.Prelude
import Data.Singletons.Sigma

import Dependent
import SingChar
import Dependent (ExtractParam1(ExtractParam1))

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)


-- Primitive Types ------------------------------------------------------------

data PrimType
  = I Nat
  | Void
  | Ptr PrimType
  | Array PrimType Natural
  {-
    I'm using `Natural` instead of Nat because I can't figure out how to 
    create a list length of type `SNat`
  -}

  | Struct Str
  | ArrStruct PrimType
  | VTable Str
  | FuncType PrimType [PrimType]

type SPrimType :: PrimType -> Type
data SPrimType t where
  SI          :: SNat n -> SPrimType (I n)
  SVoid       :: SPrimType 'Void
  SPtr        :: SPrimType t -> SPrimType (Ptr t)
  SArray      :: SPrimType t -> SNatural n -> SPrimType (Array t n)
  SStruct     :: SStr s -> SPrimType (Struct s)
  SArrStruct  :: SPrimType t -> SPrimType (ArrStruct t)
  SVTable     :: SStr s -> SPrimType (VTable s)
  SFuncType   :: SPrimType t
              -> SList (ts :: [PrimType])
              -> SPrimType (FuncType t ts)


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

deriving instance Eq (SList (ts :: [PrimType]))
deriving instance Ord (SList (ts :: [PrimType]))

-- Simple Values --------------------------------------------------------------
type Reg :: PrimType -> Type 
data Reg t where
  Reg :: String -> Int -> Reg t
  SpecialReg :: String -> Reg t

type Constant :: PrimType -> Type
data Constant t where
  Const :: String -> Int -> Constant t
  SpecialConst :: String -> Constant t


type Value :: PrimType -> Type
data Value t where
  Var       :: Reg t -> Value t
  ILit      :: Int -> Value (I n)
  BoolLit   :: Bool -> Value (I 1)
  ConstPtr  :: Constant t -> Value (Ptr t)
  Null      :: Value (Ptr t)
  FuncConst :: FuncLabel t ts -> Value (Ptr (FuncType t ts))


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

  deriving (Show, Eq, Ord)


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
  Call          :: Sing t -> Value (Ptr (FuncType t ts))
                          -> SList ts
                          -> ArgList ts
                          -> Expr t
  GetElemPtr    :: Sing t
                -> Sing (I n) -> Value (Ptr t)
                              -> Value (I n)
                              -> Expr (Ptr t)
  
  GetArrElemPtr :: Sing (Array t k)
                -> Sing (I n)
                -> Sing (I m) -> Value (Ptr (Array t k))
                              -> Value (I n)
                              -> Value (I m)
                              -> Expr (Ptr t)

  GetAttrPtr    :: Sing t1
                -> Sing (I n)
                -> Sing (I m) -> Value (Ptr t1)
                              -> Value (I n)
                              -> Value (I m)
                              -> Expr (Ptr t2)

  GetArrAttrPtr :: Sing (ArrStruct s)
                -> Sing (I n)
                -> Sing (I m) -> Value (Ptr (ArrStruct s))
                              -> Value (I n)
                              -> Value (I m)
                              -> Expr (Ptr t)

  ICMP          :: Sing t     -> CMPKind
                              -> Value t
                              -> Value t
                              -> Expr (I 1)

  Phi           :: Sing t -> [(Label, Value t)] -> Expr t

  Load :: Sing t -> Value (Ptr t) -> Expr t

  BitCast :: Sing t1 -> Value t1 -> Sing t2 -> Expr t2
  
deriving instance Show (Expr t)

-- Instructions ---------------------------------------------------------------
data SimpleInstr where
  Ass :: Reg t -> Expr t -> SimpleInstr
  VoidExpr :: Expr 'Void -> SimpleInstr
  Store :: Sing t -> Value t -> Value (Ptr t) -> SimpleInstr

data BranchInstr where
  Branch :: Label -> BranchInstr
  CondBranch :: Value (I 1) -> Label -> Label -> BranchInstr
  Ret :: Sing t -> Value t -> BranchInstr
  RetVoid :: BranchInstr

data ComSimpleInstr = Comment String | Instr SimpleInstr (Maybe String)
data ComBranchInstr = BrInstr BranchInstr (Maybe String)

deriving instance Show SimpleInstr
deriving instance Show BranchInstr
deriving instance Show ComSimpleInstr
deriving instance Show ComBranchInstr

-- Simple Block ---------------------------------------------------------------
data SimpleBlock
  = SimpleBlock 
    { label :: Label
    , body :: [ComSimpleInstr]
    , lastInstr :: ComBranchInstr
    }
  
  deriving Show


-- Functions ------------------------------------------------------------------
type ArgList ts = DList Value ts
type ParamList ts = DList Reg ts

type Func :: PrimType -> [PrimType] -> Type
data Func t ts where
  Func :: SPrimType t
        -> SList ts
        -> ParamList ts
        -> FuncLabel t ts
        -> [SimpleBlock]
        -> Func t ts

deriving instance Show (DList Value ts)
deriving instance Show (DList Reg ts)
deriving instance Show (Func t ts)


-- Program --------------------------------------------------------------------
type StrLitData :: Natural -> Type
data StrLitData n = StrLitData (Constant (Array (I 8) n)) String

type StructDef :: Str -> Type
data StructDef t where
  StructDef ::
    { structName  :: SStr t
    , attributes  :: DList SPrimType ts
    --, methods     :: [Sigma2 PrimType [PrimType] (TyCon2 Func)]
    --, methods     :: [(String, Either (Some SStr) SomeFunc)]
    , methods     ::  [(String, FuncOrParent)]
    , constructor  :: Func 'Void '[Ptr (Struct t)]
    } -> StructDef t

type FuncOrParent = Sigma2 
                      [PrimType] 
                      PrimType                        
                      (TyCon2 (ExtractParam2' (Either (Some SStr)) Func))

data LLVMProg
  = LLVM 
    { mainFunc    :: Func (I 32) '[]
    , funcs       :: [SomeFunc]
    , externFuncs :: [SomeFuncLabel]
    , strLits     :: [(String, SomeStrConst)]
    
    , customTs :: [Some StructDef]
    , mallocTs :: [Some SPrimType]
    , arrTs    :: [Some SPrimType]
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


