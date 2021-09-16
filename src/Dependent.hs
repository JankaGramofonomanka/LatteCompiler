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
  , RankNTypes
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


data Sigma2 t1 t2 :: (t1 ~> t2 ~> *) -> * where
  (:&&:) 
    :: (Sing (v1 :: t1), Sing (v2 :: t2))
    -> f @@ v1 @@ v2 -> Sigma2 t1 t2 f
infixr 5 :&&:

data Sigma3 t1 t2 t3 :: (t1 ~> t2 ~> t3 ~> *) -> * where
  (:&&&:) 
    :: (Sing (v1 :: t1), Sing (v2 :: t2), Sing (v3 :: t3))
    -> f @@ v1 @@ v2 @@ v3 -> Sigma3 t1 t2 t3 f
infixr 5 :&&&:


type Some :: (k -> Type) -> Type
data Some f where
  Some :: f v -> Some f

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

fromSList :: SList ts -> DList Sing ts
fromSList SNil = DNil
fromSList (SCons x xs) = x :> fromSList xs




newtype ExtractParam1 (a :: k1 -> *) b = ExtractParam1 (a b)
newtype ExtractParam2 (a :: k1 -> *) (b :: k2 -> k1) c
  = ExtractParam2 (a (b c))
newtype ExtractParam3 (a :: k1 -> *) (b :: k2 -> k1) (c :: k3 -> k2) d
  = ExtractParam3 (a (b (c d)))

extractParam2 :: e (a b) -> ExtractParam2 e a b
extractParam2 = ExtractParam2

insertParam2 :: ExtractParam2 e a b -> e (a b)
insertParam2 (ExtractParam2 x) = x

extractParam3 :: e (a (b c)) -> ExtractParam3 e a b c
extractParam3 = ExtractParam3

insertParam3 :: ExtractParam3 e a b c -> e (a (b c))
insertParam3 (ExtractParam3 x) = x


sLengthInt :: SList ts -> Int
sLengthInt SNil = 0
sLengthInt (SCons x xs) = 1 + sLengthInt xs

newtype ExtractParam2'
  (a :: k3 -> *)
  (b :: k1 -> k2 -> k3)
  (st :: k2)
  (ch :: k1)
  = ExtractParam2' (a (b ch st))

extractParam2' :: e (op c s) -> ExtractParam2' e op s c
extractParam2' = ExtractParam2'

insertParam2' :: ExtractParam2' e op s c -> e (op c s)
insertParam2' (ExtractParam2' x) = x
