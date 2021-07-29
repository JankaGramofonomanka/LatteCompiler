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


type Value :: PrimType -> Type
data Value t where
  Reg :: String -> Value t
  IConst :: Int -> Value (I n)

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
        -> Func t ts



