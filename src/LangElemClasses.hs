{-# LANGUAGE GADTs #-}


module LangElemClasses where

import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.PrintLatte ( printTree )
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxGADT as GS


import Position.Position ( HasPosition(position) )
import Position.SyntaxPosition
import Syntax.Debloater
import Syntax.Bloater

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
  
  
  toBNFCType :: t -> BNFC.Type

  printType :: t -> String
  printType = printTree . toBNFCType

  anyType :: t -> GS.AnyType
  anyType = debloat . (debloat :: BNFC.Type -> S.Type) . toBNFCType

  isInt, isStr, isBool, isVoid, isNull :: t -> Bool
  isInt t = case anyType t of
    GS.AnyT (GS.Int _) -> True
    _ -> False

  isStr t = case anyType t of
    GS.AnyT (GS.Str _) -> True
    _ -> False

  isBool t = case anyType t of
    GS.AnyT (GS.Bool _) -> True
    _ -> False

  isVoid t = case anyType t of
    GS.AnyT (GS.Void _) -> True
    _ -> False

  isNull t = case anyType t of
    GS.AnyT GS.NullT -> True
    _ -> False


  


instance IsType BNFC.Type where
  toBNFCType = id

instance IsType S.Type where
  toBNFCType = bloat
  anyType = debloat
  

instance IsType (GS.Type a) where
  toBNFCType = bloat . (bloat :: GS.Type a -> S.Type)

instance IsType GS.AnyType where
  toBNFCType (GS.AnyT t) = toBNFCType t
  anyType = id




-- IsVar ----------------------------------------------------------------------

class IsVar v where
  toBNFCVar :: v -> BNFC.Var
  printVar :: v -> String
  printVar = printTree . toBNFCVar

instance IsVar BNFC.Var where
  toBNFCVar = id

instance IsVar S.Var where
  toBNFCVar = bloat
    
instance IsVar (GS.Var a) where
  toBNFCVar = bloat . (bloat :: GS.Var a -> S.Var)
    


-- IsLit ----------------------------------------------------------------------
class IsLit l where
  printLit :: l -> String

instance IsLit BNFC.PInteger where
  printLit (BNFC.PInteger (_, i)) = show i

instance IsLit S.SInt where
  printLit (S.SInt _ i) = show i

instance IsLit GS.SInt where
  printLit (GS.SInt _ i) = show i


instance IsLit BNFC.PString where
  printLit (BNFC.PString (_, s)) = show s

instance IsLit S.SStr where
  printLit (S.SStr _ s) = show s

instance IsLit GS.SStr where
  printLit (GS.SStr _ s) = show s


instance IsLit BNFC.PTrue where
  printLit (BNFC.PTrue (_, _)) = show True

instance IsLit BNFC.PFalse where
  printLit (BNFC.PFalse (_, _)) = show False

instance IsLit Bool where
  printLit True = show True
  printLit False = show False



-- IsExpr ---------------------------------------------------------------------

class IsExpr e where
  toBNFCExpr :: e -> BNFC.Expr
  printExpr :: e -> String
  printExpr = printTree . toBNFCExpr

instance IsExpr BNFC.Expr where
  toBNFCExpr = id

instance IsExpr S.Expr where
  toBNFCExpr = bloat

instance IsExpr (GS.Expr a) where
  toBNFCExpr = bloat . (bloat :: GS.Expr a -> S.Expr)

-- IsOp -----------------------------------------------------------------------
class IsOp op where
  printOp :: op -> String

instance IsOp BNFC.AddOp where
  printOp = printTree
  

instance IsOp BNFC.MulOp where
  printOp = printTree

instance IsOp BNFC.RelOp where
  printOp = printTree

instance IsOp BNFC.AndOp where
  printOp = printTree

instance IsOp BNFC.OrOp  where
  printOp = printTree

instance IsOp S.BinOp where
  printOp op = case op of
    S.Plus  _ -> printTree (bloat op :: BNFC.AddOp)
    S.Minus _ -> printTree (bloat op :: BNFC.AddOp)
    S.Times _ -> printTree (bloat op :: BNFC.MulOp)
    S.Div _   -> printTree (bloat op :: BNFC.MulOp)
    S.Mod _   -> printTree (bloat op :: BNFC.MulOp)

instance IsOp S.RelOp where
  printOp = printTree . (bloat :: S.RelOp -> BNFC.RelOp)

instance IsOp S.BoolOp where
  printOp op = case op of
    S.And _ -> printTree (bloat op :: BNFC.AndOp)
    S.Or _  -> printTree (bloat op :: BNFC.OrOp)

instance IsOp GS.BinOp where
  printOp op = case op of
    GS.Plus  _ -> printTree . (bloat :: S.BinOp -> BNFC.AddOp) $ bloat op
    GS.Minus _ -> printTree . (bloat :: S.BinOp -> BNFC.AddOp) $ bloat op
    GS.Times _ -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op
    GS.Div _   -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op
    GS.Mod _   -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op

instance IsOp (GS.RelOp a) where
  printOp = printTree . (bloat :: S.RelOp -> BNFC.RelOp) . bloat

instance IsOp GS.BoolOp where
  printOp op = case op of
    GS.And _ -> printTree $ (bloat :: S.BoolOp -> BNFC.AndOp) $ bloat op
    GS.Or _  -> printTree $ (bloat :: S.BoolOp -> BNFC.OrOp)  $ bloat op


