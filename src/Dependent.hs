{-# LANGUAGE 
    GADTs
  , StandaloneKindSignatures
  , DataKinds
  , TypeFamilies
  , StandaloneDeriving
  , TypeApplications
  , TemplateHaskell
  , ScopedTypeVariables
  , PolyKinds
  , TypeOperators
#-}

module Dependent where

import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Kind ( Type )


data Natural = Zero | Succ Natural deriving (Show, Eq, Ord)
genSingletons [''Natural]
deriving instance Show (SNatural n)
deriving instance Eq (SNatural n)
deriving instance Ord (SNatural n)


data DList (f :: k -> Type) (ts :: [k]) where
  DNil :: DList f '[]
  (:>)  :: f t -> DList f ts -> DList f (t : ts)  
infixr 5 :>


data Sigma2 (t1 :: Type) (t2 :: Type) :: (t1 ~> t2 ~> *) -> * where
  (:&&:) 
    :: (Sing (v1 :: t1), Sing (v2 :: t2))
    -> f @@ v1 @@ v2 -> Sigma2 t1 t2 f
infixr 5 :&&:


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


newtype ExtractParam1 (a :: k1 -> *) b = ExtractParam1 (a b)
newtype ExtractParam2 (a :: k2 -> *) (b :: k1 -> k2) c
  = ExtractParam2 (a (b c))

extractParam2 :: e (a b) -> ExtractParam2 e a b
extractParam2 = ExtractParam2

insertParam2 :: ExtractParam2 e a b -> e (a b)
insertParam2 (ExtractParam2 x) = x


