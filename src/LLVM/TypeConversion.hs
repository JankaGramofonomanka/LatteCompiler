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
  , PolyKinds
  , UndecidableInstances

  , TemplateHaskell
#-}

module LLVM.TypeConversion where


import Unsafe.Coerce ( unsafeCoerce )

import Data.Singletons.Prelude
import Data.Singletons.TH hiding ( Void )
import Data.Kind ( Type )

import Data.GADT.Compare


import LLVM.LLVM hiding (CMPKind(..))
import Syntax.SyntaxDep
import Data.Singletons.TypeLits (SNat)

import Dependent
import SingChar


$(singletons[d|
  getPrimType :: LatteType -> PrimType
  getPrimType t = case t of
    TInt      -> I 32
    TBool     -> I 1
    TStr      -> Ptr (I 8)
    TVoid     -> Void
    Arr t     -> Ptr (ArrStruct (getPrimType t))
    Custom s  -> Ptr (Struct s)

  getPrimTypes :: [LatteType] -> [PrimType]
  getPrimTypes [] = []
  getPrimTypes (t : ts) = getPrimType t : getPrimTypes ts
  |])



type TypedIdent :: PrimType -> Type
data TypedIdent t = TypedIdent (Sing t) String Int

typedIdent :: SLatteType t -> ScopedIdent t -> TypedIdent (GetPrimType t)
typedIdent t (Scoped lvl (Ident _ s)) = TypedIdent (sGetPrimType t) s lvl
--typedIdent t (SelfAttr (Ident _ s)) = TypedIdent (sGetPrimType t) s 0


-- GEq ------------------------------------------------------------------------
instance GEq SNat where
  geq n1 n2
    = if fromSing n1 == fromSing n2 
      then 
        -- It seems impossible to proove n1 :~: n2
        Just (admit n1 n2) 
      else 
        Nothing

    where
      admit :: SNat n1 -> SNat n2 -> n1 :~: n2
      admit n1 n2 = unsafeCoerce Refl

instance GEq SNatural where
  geq SZero SZero = Just Refl
  
  geq (SSucc n1) (SSucc n2) = case geq n1 n2 of
    Nothing -> Nothing
    Just prf -> case prf of
      Refl -> Just Refl

  geq _ _ = Nothing
  


instance GEq SPrimType where
  geq (SI n1) (SI n2) = case geq n1 n2 of
    Nothing -> Nothing
    Just prf -> case prf of
      Refl -> Just Refl

  geq SVoid SVoid = Just Refl
  geq (SPtr t1) (SPtr t2) = case geq t1 t2 of
    Nothing -> Nothing
    Just prf -> case prf of
      Refl -> Just Refl

  geq (SArray t1 n1) (SArray t2 n2) = case (geq t1 t2, geq n1 n2) of
    
    (Just prfT, Just prfN) -> case (prfT, prfN) of
      (Refl, Refl) -> Just Refl
    
    _ -> Nothing
  
  geq (SStruct s1) (SStruct s2) = case geq s1 s2 of
    Just Refl -> Just Refl
    Nothing -> Nothing
  
  geq (SArrStruct t1) (SArrStruct t2) = case geq t1 t2 of

    Just Refl -> Just Refl
    Nothing -> Nothing

  geq _ _ = Nothing    

instance GEq TypedIdent where
  geq (TypedIdent t1 s1 l1) (TypedIdent t2 s2 l2) = case geq t1 t2 of
    Nothing -> Nothing
    Just prf -> if s1 == s2 && l1 == l2 then Just prf else Nothing


-- GCompare -------------------------------------------------------------------
instance GCompare SNat where
  gcompare sn1 sn2 = case geq sn1 sn2 of
    Just prf -> case prf of
      Refl -> GEQ
    Nothing -> case compare (fromSing sn1) (fromSing sn2) of
      LT -> GLT
      GT -> GGT
      EQ -> undefined

instance GCompare SNatural where
  gcompare SZero SZero = GEQ
  gcompare (SSucc _) SZero = GGT
  gcompare SZero (SSucc _) = GLT
  gcompare (SSucc n1) (SSucc n2) = case gcompare n1 n2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> GEQ

instance GCompare SPrimType where
  gcompare (SI n1) (SI n2) = case gcompare n1 n2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> GEQ

  gcompare SVoid SVoid = GEQ
  gcompare (SPtr t1) (SPtr t2) = case gcompare t1 t2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> GEQ

  gcompare (SArray t1 n1) (SArray t2 n2) = case gcompare t1 t2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> case gcompare n1 n2 of
      GLT -> GLT
      GGT -> GGT
      GEQ -> GEQ
  
  gcompare (SStruct cls1) (SStruct cls2) = case gcompare cls1 cls2 of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  
  gcompare (SArrStruct s1) (SArrStruct s2) = case gcompare s1 s2 of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
    
  gcompare SI {} _ = GLT
  
  gcompare SVoid SI {} = GGT
  gcompare SVoid _ = GLT

  gcompare SPtr {} SI {} = GGT
  gcompare SPtr {} SVoid = GGT
  gcompare SPtr {} _ = GLT
  
  gcompare SArray {} SI {} = GGT
  gcompare SArray {} SVoid {} = GGT
  gcompare SArray {} SPtr {} = GGT
  gcompare SArray {} _ = GLT

  gcompare SStruct {} SArrStruct {} = GLT
  gcompare SStruct {} SVTable {} = GLT
  gcompare SStruct {} SFuncType {} = GLT
  gcompare SStruct {} _ = GGT

  gcompare SArrStruct {} SVTable {} = GLT
  gcompare SArrStruct {} SFuncType {} = GLT
  gcompare SArrStruct {} _ = GGT

  gcompare SVTable {} SFuncType {} = GLT
  gcompare SVTable {} _ = GGT

  gcompare SFuncType {} _ = GGT


instance GCompare TypedIdent where  
  gcompare (TypedIdent t1 s1 l1) (TypedIdent t2 s2 l2)
    = case gcompare t1 t2 of      
      GEQ -> case compare s1 s2 of
        LT -> GLT
        
        EQ -> case compare l1 l2 of
          LT -> GLT
          EQ -> GEQ
          GT -> GGT

        GT -> GGT

      c   -> case compare s1 s2 of
        LT -> GLT
        
        EQ -> case compare l1 l2 of
          LT -> GLT
          EQ -> c
          GT -> GGT

        GT -> GGT


-------------------------------------------------------------------------------
instance GEq a => Eq (Some a) where
  (Some x) == (Some y) = case geq x y of
    Just _ -> True 
    Nothing -> False
  