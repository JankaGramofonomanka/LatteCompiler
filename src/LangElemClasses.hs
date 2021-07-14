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




-- IsVar ----------------------------------------------------------------------
class IsVar v where
  printVar :: v -> String

instance IsVar BNFC.Var where
  printVar var = case var of
    BNFC.Var id       -> name id
    BNFC.Member v id  -> printVar v ++ "." ++ name id
    BNFC.Elem v e     -> printVar v ++ "[_]"

instance IsVar S.Var where
  printVar var = case var of
    S.Var     p id    -> name id
    S.Fun     p id    -> name id
    S.Member  p v id  -> printVar v ++ "." ++ name id
    S.Elem    p v e   -> printVar v ++ "[_]"
    
instance IsVar (GS.Var a) where
  printVar var = case var of
    GS.Var    p id    -> name id
    GS.Fun    p id    -> name id
    GS.Member p v id  -> printVar v ++ "." ++ name id
    GS.Elem   p v e   -> printVar v ++ "[_]"
    


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

instance IsExpr BNFC.Expr where

instance IsExpr S.Expr where

instance IsExpr (GS.Expr a) where

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
