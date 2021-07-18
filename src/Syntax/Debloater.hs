{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}


module Syntax.Debloater where


import qualified FromBNFC.AbsLatte as Bloated
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxGADT as GS
import Position.Position (position)
import LangElemClasses




class ToBeDebloated bloated clean where
  debloat :: bloated -> clean


-- AbsLatte to Syntax ---------------------------------------------------------
instance ToBeDebloated Bloated.PIdent S.Ident where
  debloat (Bloated.PIdent (pos, x)) = S.Ident pos x




instance ToBeDebloated Bloated.PInteger S.SInt where
  debloat (Bloated.PInteger (pos, str)) = S.SInt pos (read str)

instance ToBeDebloated Bloated.PString S.SStr where
  debloat (Bloated.PString (pos, str)) = S.SStr pos str

instance ToBeDebloated Bloated.Program S.Program where
  debloat prog@(Bloated.Program defs)
    = S.Program (position prog) (map debloat defs)

instance ToBeDebloated Bloated.TopDef S.TopDef where
  debloat def = case def of
    Bloated.FnDef retType funcId params funcBody -> 
      S.FnDef 
        (position def)
        (debloat retType)
        (debloat funcId)
        (map debloat params)
        (debloat funcBody)

    Bloated.BaseClassDef _ classId classBody ->
      S.ClassDef (position def) (debloat classId) Nothing (debloat classBody)

    Bloated.ChildClassDef _ classId _ parentId classBody ->
      S.ClassDef 
        (position def)
        (debloat classId)
        (Just $ debloat parentId)
        (debloat classBody)


instance ToBeDebloated Bloated.Param S.Param where
  debloat (Bloated.Param t id) = S.Param (debloat t) (debloat id)


instance ToBeDebloated Bloated.Block S.Block where
  debloat block@(Bloated.Block _ stmts _)
    = S.Block (position block) (map debloat stmts)


instance ToBeDebloated Bloated.Stmt S.Stmt where
  debloat stmt = case stmt of
    Bloated.Empty _         -> S.Empty pos
    Bloated.BStmt block     -> S.BStmt pos (debloat block)
    Bloated.Decl t items _  -> S.Decl pos (debloat t) (map debloat items)
    Bloated.Ass var expr _  -> S.Ass pos (debloat var) (debloat expr)
    Bloated.Incr var _      -> S.Incr pos (debloat var)
    Bloated.Decr var _      -> S.Incr pos (debloat var)
    Bloated.Ret _ expr _    -> S.Ret pos (debloat expr)
    Bloated.VRet _ _        -> S.VRet pos
    Bloated.Cond _ expr st  -> S.Cond pos (debloat expr) (debloat st)

    Bloated.CondElse _ expr stIf _ stElse -> 
      S.CondElse pos (debloat expr) (debloat stIf) (debloat stElse)
    
    Bloated.While _ expr st -> S.While pos (debloat expr) (debloat st)
    Bloated.SExp expr _     -> S.SExp pos (debloat expr)
    
    Bloated.For _ t iterId arr st -> 
      S.For pos (debloat t) (debloat iterId) (debloat arr) (debloat st)
      

    where
      pos = position stmt



instance ToBeDebloated Bloated.Item S.Item where
  debloat (Bloated.NoInit x) = S.NoInit (debloat x)
  debloat (Bloated.Init x expr) = S.Init (debloat x) (debloat expr)

instance ToBeDebloated Bloated.Type S.Type where
  debloat t = case t of
    Bloated.Int _ -> S.Int pos
    Bloated.Str _ -> S.Str pos
    Bloated.Bool _ -> S.Bool pos
    Bloated.Void _ -> S.Void pos
    
    Bloated.Arr elemType -> S.Arr (debloat elemType)
    Bloated.Custom classId -> S.Custom (debloat classId)


    where
      pos = position t



instance ToBeDebloated Bloated.Var S.Var where
  debloat var = case var of
    Bloated.Var id -> S.Var pos (debloat id)
    Bloated.Member v id -> S.Member pos (debloat v) (debloat id)
    Bloated.Elem v expr -> S.Elem pos (debloat v) (debloat expr)

    where
      pos = position var



instance ToBeDebloated Bloated.Expr S.Expr where

  debloat expr = case expr of
    Bloated.EVar v          -> S.EVar pos (debloat v)
    Bloated.ELitInt i       -> S.ELitInt pos (debloat i)
    Bloated.ELitTrue _      -> S.ELitBool pos True
    Bloated.ELitFalse _     -> S.ELitBool pos False
    Bloated.EApp func args  -> S.EApp pos (debloat func) (map debloat args)
    Bloated.EString s       -> S.EString pos (debloat s)
    Bloated.Neg _ e         -> S.Neg pos (debloat e)
    Bloated.Not _ e         -> S.Not pos (debloat e)
    Bloated.EMul e1 op e2   -> S.EOp pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EAdd e1 op e2   -> S.EOp pos (debloat op) (debloat e1) (debloat e2)
    Bloated.ERel e1 op e2   -> S.ERel pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EAnd e1 op e2   -> S.EBool pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EOr e1 op e2    -> S.EBool pos (debloat op) (debloat e1) (debloat e2)
    Bloated.NewArr _ t e    -> S.NewArr pos (debloat t) (debloat e)
    Bloated.NewObj _ clsId  -> S.NewObj pos (debloat clsId)
    Bloated.Cast t e        -> S.Cast pos (debloat t) (debloat e)
    
    Bloated.CastE (Bloated.EVar (Bloated.Var (Bloated.PIdent (p, id)))) e ->
      S.Cast pos (S.Custom (S.Ident p id)) (debloat e)
    
    Bloated.CastE e1 e2 -> error $ "parse error at " ++ show (position e1)

    where pos = position expr



instance ToBeDebloated Bloated.AddOp S.BinOp where
  debloat op = case op of
    Bloated.Plus _ -> S.Plus pos
    Bloated.Minus _ -> S.Minus pos

    where 
      pos = position op

instance ToBeDebloated Bloated.MulOp S.BinOp where
  debloat op = case op of
    Bloated.Times _ -> S.Times pos
    Bloated.Div _   -> S.Div pos
    Bloated.Mod _   -> S.Mod pos

    where 
      pos = position op


instance ToBeDebloated Bloated.RelOp S.RelOp where
  debloat op = case op of
    Bloated.LTH _ -> S.LTH pos
    Bloated.LE  _ -> S.LE  pos
    Bloated.GTH _ -> S.GTH pos
    Bloated.GE  _ -> S.GE  pos
    Bloated.EQU _ -> S.EQU pos
    Bloated.NE  _ -> S.NE  pos

    where
      pos = position op


instance ToBeDebloated Bloated.AndOp S.BoolOp where
  debloat op@(Bloated.And _) = S.And (position op)

instance ToBeDebloated Bloated.OrOp S.BoolOp where
  debloat op@(Bloated.Or _) = S.Or (position op)


instance ToBeDebloated Bloated.ClassBody S.ClassBody where
  debloat body@(Bloated.ClassBody _ memberDecls _)
    = S.ClassBody pos (map debloat memberDecls)

    where pos = position body


instance ToBeDebloated Bloated.MemberDecl S.MemberDecl where
  debloat memberDecl = case memberDecl of
    Bloated.AttrDecl t attrId _ -> S.AttrDecl pos (debloat t) (debloat attrId)
    Bloated.MethodDecl def -> S.MethodDecl (debloat def)

  
    where pos = position memberDecl

-- Syntax to SyntaxGADT -------------------------------------------------------
-- only some elements can be easily "debloated"

instance ToBeDebloated S.Ident (GS.Ident a) where
  debloat (S.Ident pos x) = GS.Ident pos x




instance ToBeDebloated S.SInt GS.SInt where
  debloat (S.SInt pos int) = GS.SInt pos int

instance ToBeDebloated S.SStr GS.SStr where
  debloat (S.SStr pos str) = GS.SStr pos str


instance ToBeDebloated S.BinOp GS.BinOp where
  debloat op = case op of
    S.Plus  p -> GS.Plus  p
    S.Minus p -> GS.Minus p
    S.Times p -> GS.Times p
    S.Div   p -> GS.Div   p
    S.Mod   p -> GS.Mod   p


instance ToBeDebloated S.BoolOp GS.BoolOp where
  debloat op = case op of
    S.And p -> GS.And p
    S.Or  p -> GS.Or  p

instance ToBeDebloated S.Param GS.Param where
  debloat (S.Param t id) = case anyType t of
    GS.AnyT tt -> GS.Param tt (debloat id)


bloatId :: GS.Ident a -> S.Ident
bloatId (GS.Ident p s) = S.Ident p s

