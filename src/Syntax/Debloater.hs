{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Syntax.Debloater where


import qualified FromBNFC.AbsLatte as Bloated
import Syntax.Syntax
import Position (position)




class ToBeDebloated bloated clean where
  debloat :: bloated -> clean


  
instance ToBeDebloated Bloated.PIdent Ident where
  debloat (Bloated.PIdent (pos, x)) = Ident pos x




instance ToBeDebloated Bloated.PInteger SInt where
  debloat (Bloated.PInteger (pos, str)) = SInt pos (read str)

instance ToBeDebloated Bloated.PString SStr where
  debloat (Bloated.PString (pos, str)) = SStr pos str

instance ToBeDebloated Bloated.Program Program where
  debloat prog@(Bloated.Program defs)
    = Program (position prog) (map debloat defs)

instance ToBeDebloated Bloated.TopDef TopDef where
  debloat def = case def of
    Bloated.FnDef retType funcId params funcBody -> 
      FnDef 
        (position def)
        (debloat retType)
        (debloat funcId)
        (map debloat params)
        (debloat funcBody)

    Bloated.BaseClassDef _ classId classBody ->
      ClassDef (position def) (debloat classId) Nothing (debloat classBody)

    Bloated.ChildClassDef _ classId _ parentId classBody ->
      ClassDef 
        (position def)
        (debloat classId)
        (Just $ debloat parentId)
        (debloat classBody)


instance ToBeDebloated Bloated.Param Param where
  debloat (Bloated.Param t id) = Param (debloat t) (debloat id)


instance ToBeDebloated Bloated.Block Block where
  debloat block@(Bloated.Block _ stmts _)
    = Block (position block) (map debloat stmts)


instance ToBeDebloated Bloated.Stmt Stmt where
  debloat stmt = case stmt of
    Bloated.Empty _         -> Empty pos
    Bloated.BStmt block     -> BStmt pos (debloat block)
    Bloated.Decl t items _  -> Decl pos (debloat t) (map debloat items)
    Bloated.Ass var expr _  -> Ass pos (debloat var) (debloat expr)
    Bloated.Incr var _      -> Incr pos (debloat var)
    Bloated.Decr var _      -> Incr pos (debloat var)
    Bloated.Ret _ expr _    -> Ret pos (debloat expr)
    Bloated.VRet _ _        -> VRet pos
    Bloated.Cond _ expr st  -> Cond pos (debloat expr) (debloat st)

    Bloated.CondElse _ expr stIf _ stElse -> 
      CondElse pos (debloat expr) (debloat stIf) (debloat stElse)
    
    Bloated.While _ expr st -> While pos (debloat expr) (debloat st)
    Bloated.SExp expr _     -> SExp pos (debloat expr)
    
    Bloated.For _ t iterId arr st -> 
      For pos (debloat t) (debloat iterId) (debloat arr) (debloat st)
      

    where
      pos = position stmt



instance ToBeDebloated Bloated.Item Item where
  debloat (Bloated.NoInit x) = NoInit (debloat x)
  debloat (Bloated.Init x expr) = Init (debloat x) (debloat expr)

instance ToBeDebloated Bloated.Type Type where
  debloat t = case t of
    Bloated.Int _ -> Int pos
    Bloated.Str _ -> Str pos
    Bloated.Bool _ -> Bool pos
    Bloated.Void _ -> Void pos
    
    Bloated.Arr elemType -> Arr (debloat elemType)
    Bloated.Custom classId -> Custom (debloat classId)


    where
      pos = position t



instance ToBeDebloated Bloated.Var Var where
  debloat var = case var of
    Bloated.Var id -> Var pos (debloat id)
    Bloated.Member v id -> Member pos (debloat v) (debloat id)
    Bloated.Elem v expr -> Elem pos (debloat v) (debloat expr)

    where
      pos = position var



instance ToBeDebloated Bloated.Expr Expr where

  debloat expr = case expr of
    Bloated.EVar v          -> EVar pos (debloat v)
    Bloated.ELitInt i       -> ELitInt pos (debloat i)
    Bloated.ELitTrue _      -> ELitBool pos True
    Bloated.ELitFalse _     -> ELitBool pos False
    Bloated.EApp func args  -> EApp pos (debloat func) (map debloat args)
    Bloated.EString s       -> EString pos (debloat s)
    Bloated.Neg _ e         -> Neg pos (debloat e)
    Bloated.Not _ e         -> Not pos (debloat e)
    Bloated.EMul e1 op e2   -> EOp pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EAdd e1 op e2   -> EOp pos (debloat op) (debloat e1) (debloat e2)
    Bloated.ERel e1 op e2   -> ERel pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EAnd e1 op e2   -> EBool pos (debloat op) (debloat e1) (debloat e2)
    Bloated.EOr e1 op e2    -> EBool pos (debloat op) (debloat e1) (debloat e2)
    Bloated.NewArr _ t e    -> NewArr pos (debloat t) (debloat e)
    Bloated.NewObj _ clsId  -> NewObj pos (debloat clsId)
    Bloated.Cast t e        -> Cast pos (debloat t) (debloat e)
    
    Bloated.CastE e1 e2 -> error "not a type"

    where pos = position expr



instance ToBeDebloated Bloated.AddOp BinOp where
  debloat op = case op of
    Bloated.Plus _ -> Plus pos
    Bloated.Minus _ -> Minus pos

    where 
      pos = position op

instance ToBeDebloated Bloated.MulOp BinOp where
  debloat op = case op of
    Bloated.Times _ -> Times pos
    Bloated.Div _   -> Div pos
    Bloated.Mod _   -> Mod pos

    where 
      pos = position op


instance ToBeDebloated Bloated.RelOp RelOp where
  debloat op = case op of
    Bloated.LTH _ -> LTH pos
    Bloated.LE  _ -> LE  pos
    Bloated.GTH _ -> GTH pos
    Bloated.GE  _ -> GE  pos
    Bloated.EQU _ -> EQU pos
    Bloated.NE  _ -> NE  pos

    where
      pos = position op


instance ToBeDebloated Bloated.AndOp BoolOp where
  debloat op@(Bloated.And _) = And (position op)

instance ToBeDebloated Bloated.OrOp BoolOp where
  debloat op@(Bloated.Or _) = Or (position op)


instance ToBeDebloated Bloated.ClassBody ClassBody where
  debloat body@(Bloated.ClassBody _ memberDecls _)
    = ClassBody pos (map debloat memberDecls)

    where pos = position body


instance ToBeDebloated Bloated.MemberDecl MemberDecl where
  debloat memberDecl = case memberDecl of
    Bloated.AttrDecl t attrId _ -> AttrDecl pos (debloat t) (debloat attrId)
    Bloated.MethodDecl def -> MethodDecl (debloat def)

  
    where pos = position memberDecl
























