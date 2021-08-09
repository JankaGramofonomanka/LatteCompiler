{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}


module Syntax.DebloaterDep where

import Data.Singletons

import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import Position.Position (position)
import Syntax.Debloater
import Dependent
import SingChar



-- Syntax to SyntaxGADT -------------------------------------------------------
-- only some elements can be easily "debloated"

instance ToBeDebloated S.Ident (DS.Ident a) where
  debloat (S.Ident pos x) = DS.Ident pos x

instance ToBeDebloated S.Ident (DS.FuncIdent t ts) where
  debloat (S.Ident pos x) = DS.FuncIdent pos x

instance ToBeDebloated S.Ident (DS.ClassIdent cls) where
  debloat (S.Ident pos x) = DS.ClassIdent pos x




instance ToBeDebloated S.BinOp DS.BinOp where
  debloat op = case op of
    S.Plus  p -> DS.Plus  p
    S.Minus p -> DS.Minus p
    S.Times p -> DS.Times p
    S.Div   p -> DS.Div   p
    S.Mod   p -> DS.Mod   p


instance ToBeDebloated S.BoolOp DS.BoolOp where
  debloat op = case op of
    S.And p -> DS.And p
    S.Or  p -> DS.Or  p


instance ToBeDebloated S.Param (Some DS.Param) where
  debloat (S.Param t id) = case debloat t of
    Some tt -> Some $ DS.Param tt (debloat id)

instance ToBeDebloated S.Type DS.LatteType where
  debloat t = case t of
    S.Int p           -> DS.TInt
    S.Str p           -> DS.TStr
    S.Bool p          -> DS.TBool
    S.Void p          -> DS.TVoid
    S.Arr elemType    -> DS.Arr (debloat elemType)
      
    S.Custom (S.Ident p id) -> DS.Custom $ fromString id

instance ToBeDebloated S.Type (Some DS.SLatteType) where
  debloat t = case toSing (debloat t) of
    SomeSing tt -> Some tt

instance ToBeDebloated S.Type (Some DS.TypeKW) where
  debloat t = case t of
    S.Int p           -> Some $ DS.KWInt p
    S.Str p           -> Some $ DS.KWStr p
    S.Bool p          -> Some $ DS.KWBool p
    S.Void p          -> Some $ DS.KWVoid p
    S.Arr elemType    -> case debloat elemType of
      Some elemT -> Some $ DS.KWArr elemT
      
    S.Custom id -> Some $ DS.KWCustom (debloat id)

bloatId :: DS.Ident a -> S.Ident
bloatId (DS.Ident p s) = S.Ident p s

