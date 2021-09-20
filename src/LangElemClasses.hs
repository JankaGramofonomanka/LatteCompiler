{-# LANGUAGE
    GADTs
  , FlexibleInstances
#-}


module LangElemClasses where

import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.PrintLatte ( printTree )
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import qualified LLVM.TypeConversion as LLVM


import Position.Position ( HasPosition(position) )
import Position.SyntaxPosition
import Syntax.Debloater
import Syntax.Bloater
import Dependent
import SingChar

ticks :: String -> String
ticks s = "`" ++ s ++ "`"

-- IsIdent --------------------------------------------------------------------
class IsIdent a where

  name :: a -> String
  printIdent :: a -> String
  printIdent = ticks . name

instance IsIdent BNFC.PIdent where
  name (BNFC.PIdent (_, s)) = s

instance IsIdent S.Ident where
  name (S.Ident _ s) = s

instance IsIdent (DS.Ident a) where
  name (DS.Ident _ s) = s

instance IsIdent (DS.ScopedIdent a) where
  name (DS.Scoped _ x) = name x


instance IsIdent (DS.FuncIdent t ts) where
  name (DS.FuncIdent _ s) = s

instance IsIdent (DS.ClassIdent cls) where
  name (DS.ClassIdent _ s) = singToString s

instance IsIdent (LLVM.TypedIdent t) where
  name (LLVM.TypedIdent t s l) = s


-- IsType ---------------------------------------------------------------------


class IsType t where
  
  
  toBNFCType :: t -> BNFC.Type

  printType :: t -> String
  printType = ticks . printTree . toBNFCType

  someType :: t -> Some DS.SLatteType
  someType = debloat . (debloat :: BNFC.Type -> S.Type) . toBNFCType

  isInt, isStr, isBool, isVoid, isNull :: t -> Bool
  isInt t = case someType t of
    Some DS.STInt -> True
    _ -> False

  isStr t = case someType t of
    Some DS.STStr -> True
    _ -> False

  isBool t = case someType t of
    Some DS.STBool -> True
    _ -> False

  isVoid t = case someType t of
    Some DS.STVoid -> True
    _ -> False

  isNull t = case someType t of
    Some DS.STNull -> True
    _ -> False


  


instance IsType BNFC.Type where
  toBNFCType = id

instance IsType S.Type where
  toBNFCType = bloat
  

instance IsType (DS.TypeKW t) where
  toBNFCType = bloat . (bloat :: DS.TypeKW t -> S.Type)

instance IsType (Some DS.TypeKW) where
  toBNFCType (Some t) = toBNFCType t

instance IsType DS.LatteType where
  toBNFCType = bloat . (bloat :: DS.LatteType -> S.Type)

instance IsType (DS.SLatteType t) where
  toBNFCType = bloat . (bloat :: DS.SLatteType t -> S.Type)
  someType = Some

instance IsType (Some DS.SLatteType) where
  toBNFCType (Some t) = toBNFCType t
  someType = id

-- IsVar ----------------------------------------------------------------------

class IsVar v where
  toBNFCVar :: v -> BNFC.Var
  printVar :: v -> String
  printVar = ticks . printTree . toBNFCVar

instance IsVar BNFC.Var where
  toBNFCVar = id

instance IsVar S.Var where
  toBNFCVar = bloat
    
instance IsVar (DS.Var a) where
  toBNFCVar = bloat . (bloat :: DS.Var a -> S.Var)
    


-- IsLit ----------------------------------------------------------------------
class IsLit l where
  prtLit :: l -> String
  printLit :: l -> String
  printLit = ticks . prtLit

instance IsLit BNFC.PInteger where
  prtLit (BNFC.PInteger (_, i)) = show i

instance IsLit S.SInt where
  prtLit (S.SInt _ i) = show i

instance IsLit BNFC.PString where
  prtLit (BNFC.PString (_, s)) = show s

instance IsLit S.SStr where
  prtLit (S.SStr _ s) = show s

instance IsLit BNFC.PTrue where
  prtLit (BNFC.PTrue (_, _)) = show True

instance IsLit BNFC.PFalse where
  prtLit (BNFC.PFalse (_, _)) = show False

instance IsLit Bool where
  prtLit True = show True
  prtLit False = show False



-- IsExpr ---------------------------------------------------------------------

class IsExpr e where
  toBNFCExpr :: e -> BNFC.Expr
  printExpr :: e -> String
  printExpr = ticks . printTree . toBNFCExpr

instance IsExpr BNFC.Expr where
  toBNFCExpr = id

instance IsExpr S.Expr where
  toBNFCExpr = bloat

instance IsExpr (DS.Expr a) where
  toBNFCExpr = bloat . (bloat :: DS.Expr a -> S.Expr)

-- IsOp -----------------------------------------------------------------------
class IsOp op where
  prtOp :: op -> String
  printOp :: op -> String
  printOp = ticks . prtOp

instance IsOp BNFC.AddOp where
  prtOp = printTree
  

instance IsOp BNFC.MulOp where
  prtOp = printTree

instance IsOp BNFC.RelOp where
  prtOp = printTree

instance IsOp BNFC.AndOp where
  prtOp = printTree

instance IsOp BNFC.OrOp  where
  prtOp = printTree

instance IsOp S.BinOp where
  prtOp op = case op of
    S.Plus  _ -> printTree (bloat op :: BNFC.AddOp)
    S.Minus _ -> printTree (bloat op :: BNFC.AddOp)
    S.Times _ -> printTree (bloat op :: BNFC.MulOp)
    S.Div _   -> printTree (bloat op :: BNFC.MulOp)
    S.Mod _   -> printTree (bloat op :: BNFC.MulOp)

instance IsOp S.RelOp where
  prtOp = printTree . (bloat :: S.RelOp -> BNFC.RelOp)

instance IsOp S.BoolOp where
  prtOp op = case op of
    S.And _ -> printTree (bloat op :: BNFC.AndOp)
    S.Or _  -> printTree (bloat op :: BNFC.OrOp)

instance IsOp DS.BinOp where
  prtOp op = case op of
    DS.Plus  _ -> printTree . (bloat :: S.BinOp -> BNFC.AddOp) $ bloat op
    DS.Minus _ -> printTree . (bloat :: S.BinOp -> BNFC.AddOp) $ bloat op
    DS.Times _ -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op
    DS.Div _   -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op
    DS.Mod _   -> printTree . (bloat :: S.BinOp -> BNFC.MulOp) $ bloat op

instance IsOp (DS.RelOp a) where
  prtOp = printTree . (bloat :: S.RelOp -> BNFC.RelOp) . bloat

instance IsOp DS.BoolOp where
  prtOp op = case op of
    DS.And _ -> printTree $ (bloat :: S.BoolOp -> BNFC.AndOp) $ bloat op
    DS.Or _  -> printTree $ (bloat :: S.BoolOp -> BNFC.OrOp)  $ bloat op


-- IsStmt ---------------------------------------------------------------------
class IsStmt s where
  toBNFCStmt :: s -> BNFC.Stmt
  printStmt :: s -> String
  printStmt s = printTree (toBNFCStmt s)

instance IsStmt BNFC.Stmt where
  toBNFCStmt = id

instance IsStmt S.Stmt where
  toBNFCStmt = bloat

instance IsStmt DS.Stmt where
  toBNFCStmt = bloat . (bloat :: DS.Stmt -> S.Stmt)



  