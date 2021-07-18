{-# LANGUAGE GADTs #-}


module LangElemClasses where

import qualified FromBNFC.AbsLatte as BNFC
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

printAnyType :: GS.AnyType -> String
printAnyType t = case t of
    GS.AnyT (GS.Int _)          -> "int"
    GS.AnyT (GS.Str _)          -> "string"
    GS.AnyT (GS.Bool _)         -> "boolean"
    GS.AnyT (GS.Void _)         -> "void"
    GS.AnyT (GS.Arr elemType)   -> printAnyType (GS.AnyT elemType) ++ "[]"
    GS.AnyT (GS.Custom classId) -> name classId
    GS.AnyT GS.NullT            -> "null"

class IsType t where
  
  anyType :: t -> GS.AnyType
  printType :: t -> String
  printType = printAnyType . anyType

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
  anyType t = case t of
    BNFC.Int _        -> GS.AnyT (GS.Int pos)
    BNFC.Str _        -> GS.AnyT (GS.Str pos)
    BNFC.Bool _       -> GS.AnyT (GS.Bool pos)
    BNFC.Void _       -> GS.AnyT (GS.Void pos)
    BNFC.Arr elemType -> case anyType elemType of
                          GS.AnyT elemT -> GS.AnyT (GS.Arr elemT)

    BNFC.Custom (BNFC.PIdent (p, id)) -> GS.AnyT $ GS.Custom $ GS.Ident p id

    where
      pos = position t


instance IsType S.Type where
  anyType = debloat
  

instance IsType (GS.Type a) where
  anyType = GS.AnyT

instance IsType GS.AnyType where
  anyType = id




-- IsVar ----------------------------------------------------------------------
printSVar :: S.Var -> String
printSVar var = case var of
    S.Var     p id    -> name id
    S.Member  p e id  -> printExpr e ++ "." ++ name id
    S.Elem    p e1 e2 -> printExpr e1 ++ "[" ++ printExpr e2 ++ "]"
    S.Null    p       -> "null"

class IsVar v where
  toSVar :: v -> S.Var
  printVar :: v -> String
  printVar = printSVar . toSVar

instance IsVar BNFC.Var where
  toSVar = debloat

instance IsVar S.Var where
  toSVar = id
    
instance IsVar (GS.Var a) where
  toSVar = bloat
    


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
printSExpr :: S.Expr -> String
printSExpr e = "_"

class IsExpr e where
  toSExpr :: e -> S.Expr
  printExpr :: e -> String
  printExpr = printSExpr . toSExpr

instance IsExpr BNFC.Expr where
  toSExpr = debloat

instance IsExpr S.Expr where
  toSExpr = id

instance IsExpr (GS.Expr a) where
  toSExpr = bloat

-- IsOp -----------------------------------------------------------------------
class IsOp op where
  printOp :: op -> String

instance IsOp BNFC.AddOp where
  printOp op = case op of
    BNFC.Plus  _ -> "+"
    BNFC.Minus _ -> "-"
  

instance IsOp BNFC.MulOp where
  printOp op = case op of
    BNFC.Times _  -> "*"
    BNFC.Div _    -> "/"
    BNFC.Mod _    -> "%"

instance IsOp BNFC.RelOp where
  printOp op = case op of
    BNFC.LTH  _ -> "<"
    BNFC.LE   _ -> "<="
    BNFC.GTH  _ -> ">"
    BNFC.GE   _ -> ">="
    BNFC.EQU  _ -> "=="
    BNFC.NE   _ -> "!="

instance IsOp BNFC.AndOp where
  printOp op = case op of
    BNFC.And _ -> "&&"

instance IsOp BNFC.OrOp  where
  printOp op = case op of
    BNFC.Or _ -> "||"

instance IsOp S.BinOp where
  printOp op = case op of
    S.Plus  _ -> "+"
    S.Minus _ -> "-"
    S.Times _  -> "*"
    S.Div _    -> "/"
    S.Mod _    -> "%"

instance IsOp S.RelOp where
  printOp op = case op of
    S.LTH  _ -> "<"
    S.LE   _ -> "<="
    S.GTH  _ -> ">"
    S.GE   _ -> ">="
    S.EQU  _ -> "=="
    S.NE   _ -> "!="

instance IsOp S.BoolOp where
  printOp op = case op of
    S.And _ -> "&&"
    S.Or _  -> "||"

instance IsOp GS.BinOp where
  printOp op = case op of
    GS.Plus  _ -> "+"
    GS.Minus _ -> "-"
    GS.Times _  -> "*"
    GS.Div _    -> "/"
    GS.Mod _    -> "%"

instance IsOp (GS.RelOp a) where
  printOp op = case op of
    GS.LTH  _ -> "<"
    GS.LE   _ -> "<="
    GS.GTH  _ -> ">"
    GS.GE   _ -> ">="
    GS.EQU  _ -> "=="
    GS.NE   _ -> "!="

instance IsOp GS.BoolOp where
  printOp op = case op of
    GS.And _ -> "&&"
    GS.Or _  -> "||"


