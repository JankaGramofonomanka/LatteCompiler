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

  , EmptyCase
#-}

module LLVM.TypeConversion where




import Data.Singletons.Prelude
import Data.Singletons.TH hiding ( Void )
import Data.Kind ( Type )

import Data.GADT.Compare


import LLVM.LLVM hiding (CMPKind(..))
import qualified Syntax.SyntaxGADT as S
import Data.Singletons.TypeLits (SNat)




type family GetPrimType (t :: Type) :: PrimType where
  GetPrimType Int     = I 32
  GetPrimType Bool    = I 1
  GetPrimType String  = Ptr (I 8)
  GetPrimType S.Void  = Void
  --GetPrimType (S.Arr t) = Ptr (GetPrimType t)

type TypedIdent :: PrimType -> Type
data TypedIdent t where
  TypedIdent :: Sing t -> String -> TypedIdent t

typedIdent :: S.Type t -> S.Ident t -> TypedIdent (GetPrimType t)
typedIdent t (S.Ident _ s) = TypedIdent st s

  where 
    
    st = case t of
      S.Int _   -> sing @(I 32)
      S.Str _   -> sing @(Ptr (I 8))
      S.Bool _  -> sing @(I 1)
      S.Void _  -> sing @Void
      _         -> undefined
  
      --Arr     :: Eq b => Type b -> Type (Array b)
      --Custom  :: Ident Class -> Type Custom
      --NullT   :: Type Null

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
      admit n1 n2 = case () of {}

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

  geq (SArr t1 n1) (SArr t2 n2) = case (geq t1 t2, geq n1 n2) of
    
    (Just prfT, Just prfN) -> case (prfT, prfN) of
      (Refl, Refl) -> Just Refl
    
    _ -> Nothing
    
  geq _ _ = Nothing    

instance GEq TypedIdent where
  geq (TypedIdent t1 s1) (TypedIdent t2 s2) = case geq t1 t2 of
    Nothing -> Nothing
    Just prf -> if s1 == s2 then Just prf else Nothing


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

  gcompare (SArr t1 n1) (SArr t2 n2) = case gcompare t1 t2 of
    GLT -> GLT
    GGT -> GGT
    GEQ -> case gcompare n1 n2 of
      GLT -> GLT
      GGT -> GGT
      GEQ -> GEQ
    
  gcompare SI {} _ = GLT
  
  gcompare SVoid SI {} = GGT
  gcompare SVoid _ = GLT

  gcompare SPtr  {} SArr {} = GLT
  gcompare SPtr  {} _ = GGT
  
  gcompare SArr  {} _ = GGT


instance GCompare TypedIdent where  
  gcompare (TypedIdent t1 s1) (TypedIdent t2 s2)
    = case gcompare t1 t2 of      
      GEQ -> case compare s1 s2 of
        LT -> GLT
        EQ -> GEQ
        GT -> GGT

      c   -> case compare s1 s2 of
        LT -> GLT
        EQ -> c
        GT -> GGT


