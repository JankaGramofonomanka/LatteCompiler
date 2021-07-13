{-# LANGUAGE GADTs #-}

module LangElemClasses where

import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxGADT as GS

-- IsIdent --------------------------------------------------------------------
class IsIdent a where

  name :: a -> String

instance IsIdent BNFC.PIdent where
  name (BNFC.PIdent (_, s)) = s

instance IsIdent S.Ident where
  name (S.Ident _ s) = s

instance IsIdent (GS.Ident a) where
  name (GS.Ident _ s) = s



-- IsType ---------------------------------------------------------------------
class IsType t where
  toStr :: t -> String

instance IsType BNFC.Type where
  toStr t = case t of
    BNFC.Int _          -> "int"
    BNFC.Str _          -> "string"
    BNFC.Bool _         -> "boolean"
    BNFC.Void _         -> "void"
    BNFC.Arr elemType   -> toStr elemType ++ "[]"
    BNFC.Custom classId -> name classId


instance IsType S.Type where
  toStr t = case t of
    S.Int _           -> "int"
    S.Str _           -> "string"
    S.Bool _          -> "boolean"
    S.Void _          -> "void"
    S.Arr elemType    -> toStr elemType ++ "[]"
    S.Custom classId  -> name classId

instance IsType (GS.Type a) where
  toStr t = case t of
    GS.Int _           -> "int"
    GS.Str _           -> "string"
    GS.Bool _          -> "boolean"
    GS.Void _          -> "void"
    GS.Arr elemType    -> toStr elemType ++ "[]"
    GS.Custom classId  -> name classId









