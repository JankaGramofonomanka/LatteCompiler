{-# LANGUAGE 
    FlexibleContexts
  , RecordWildCards
  , GADTs
  , DataKinds
  , RankNTypes
  , KindSignatures
  , FlexibleInstances
  , PolyKinds
#-}

module TypeCheck.Castable where

import Data.Kind ( Type )

import Data.Singletons.Prelude

import Syntax.SyntaxDep
import Position.Position
import Position.SyntaxDepPosition
import Dependent

class MkLatteType (a :: k -> LatteType) where
  mkSing :: Sing (x :: k) -> SLatteType (a x)

instance MkLatteType Arr where
  mkSing t = SArr t

instance MkLatteType Custom where
  mkSing = SCustom

class Castable (e :: LatteType -> Type) where
  cast :: SLatteType t1 -> e t2 -> e t1

instance Castable Expr where
  cast t expr = Cast p (kwFromSing p t) expr where
    p = position expr

instance (Castable e, MkLatteType a) => Castable (ExtractParam2 e a) where
  cast t (ExtractParam2 x) = ExtractParam2 (cast (mkSing t) x)

