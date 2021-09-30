{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}


module Syntax.Debloater where

import Data.Singletons

import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import Position.Position (position)
import Position.EndPosition
import Dependent
import SingChar

class ToBeDebloated bloated clean where
  debloat :: bloated -> clean


-- AbsLatte to Syntax ---------------------------------------------------------
instance ToBeDebloated BNFC.PIdent S.Ident where
  debloat (BNFC.PIdent (pos, x)) = S.Ident pos x




instance ToBeDebloated BNFC.PInteger S.SInt where
  debloat (BNFC.PInteger (pos, str)) = S.SInt pos (read str)

instance ToBeDebloated BNFC.PString S.SStr where
  debloat (BNFC.PString (pos, str)) = S.SStr pos (stripQuotes str)
    
    where
      stripQuotes s = init (tail s)

instance ToBeDebloated BNFC.Program S.Program where
  debloat prog@(BNFC.Program defs)
    = S.Program (position prog) (map debloat defs)

instance ToBeDebloated BNFC.TopDef S.TopDef where
  debloat def = case def of
    BNFC.FnDef retType funcId params funcBody -> 
      S.FnDef 
        (position def)
        (debloat retType)
        (debloat funcId)
        (map debloat params)
        (debloat funcBody)

    BNFC.BaseClassDef _ classId classBody ->
      S.ClassDef (position def) (debloat classId) Nothing (debloat classBody)

    BNFC.ChildClassDef _ classId _ parentId classBody ->
      S.ClassDef 
        (position def)
        (debloat classId)
        (Just $ debloat parentId)
        (debloat classBody)


instance ToBeDebloated BNFC.Param S.Param where
  debloat (BNFC.Param t id) = S.Param (debloat t) (debloat id)


instance ToBeDebloated BNFC.Block S.Block where
  debloat block@(BNFC.Block _ stmts _)
    = S.Block (position block) (endPosition block) (map debloat stmts)


instance ToBeDebloated BNFC.Stmt S.Stmt where
  debloat stmt = case stmt of
    BNFC.Empty _         -> S.Empty pos
    BNFC.BStmt block     -> S.BStmt pos (debloat block)
    BNFC.Decl t items _  -> S.Decl pos (debloat t) (map debloat items)
    BNFC.Ass var expr _  -> S.Ass pos (debloat var) (debloat expr)
    BNFC.Incr var _      -> S.Incr pos (debloat var)
    BNFC.Decr var _      -> S.Decr pos (debloat var)
    BNFC.Ret _ expr _    -> S.Ret pos (debloat expr)
    BNFC.VRet _ _        -> S.VRet pos
    BNFC.Cond _ expr st  -> S.Cond pos (debloat expr) (debloat st)

    BNFC.CondElse _ expr stIf _ stElse -> 
      S.CondElse pos (debloat expr) (debloat stIf) (debloat stElse)
    
    BNFC.While _ expr st -> S.While pos (debloat expr) (debloat st)
    BNFC.SExp expr _     -> S.SExp pos (debloat expr)
    
    BNFC.For _ t iterId arr st -> 
      S.For pos (debloat t) (debloat iterId) (debloat arr) (debloat st)
      

    where
      pos = position stmt



instance ToBeDebloated BNFC.Item S.Item where
  debloat (BNFC.NoInit x) = S.NoInit (debloat x)
  debloat (BNFC.Init x expr) = S.Init (debloat x) (debloat expr)

instance ToBeDebloated BNFC.Type S.Type where
  debloat t = case t of
    BNFC.Int _ -> S.Int pos
    BNFC.Str _ -> S.Str pos
    BNFC.Bool _ -> S.Bool pos
    BNFC.Void _ -> S.Void pos
    
    BNFC.Arr elemType -> S.Arr (debloat elemType)
    BNFC.Custom classId -> S.Custom (debloat classId)


    where
      pos = position t



instance ToBeDebloated BNFC.Var S.Var where
  debloat var = case var of
    BNFC.Var id       -> S.Var pos (debloat id)
    BNFC.Member e id  -> S.Member pos (debloat e) (debloat id)
    BNFC.Elem e expr  -> S.Elem pos (debloat e) (debloat expr)
    BNFC.Null kw      -> S.Null pos
    BNFC.Self kw      -> S.Self pos

    where
      pos = position var



instance ToBeDebloated BNFC.Expr S.Expr where

  debloat expr = case expr of
    BNFC.EVar v          -> S.EVar pos (debloat v)
    BNFC.ELitInt i       -> S.ELitInt pos (debloat i)
    BNFC.ELitTrue _      -> S.ELitBool pos True
    BNFC.ELitFalse _     -> S.ELitBool pos False
    BNFC.EApp func args  -> S.EApp pos (debloat func) (map debloat args)
    BNFC.EString s       -> S.EString pos (debloat s)
    BNFC.Neg _ e         -> S.Neg pos (debloat e)
    BNFC.Not _ e         -> S.Not pos (debloat e)
    BNFC.EMul e1 op e2   -> S.EOp pos (debloat op) (debloat e1) (debloat e2)
    BNFC.EAdd e1 op e2   -> S.EOp pos (debloat op) (debloat e1) (debloat e2)
    BNFC.ERel e1 op e2   -> S.ERel pos (debloat op) (debloat e1) (debloat e2)
    BNFC.EAnd e1 op e2   -> S.EBool pos (debloat op) (debloat e1) (debloat e2)
    BNFC.EOr e1 op e2    -> S.EBool pos (debloat op) (debloat e1) (debloat e2)
    BNFC.NewArr _ t e    -> S.NewArr pos (debloat t) (debloat e)
    BNFC.NewObj _ clsId  -> S.NewObj pos (debloat clsId)
    BNFC.Cast t e        -> S.Cast pos (debloat t) (debloat e)
    
    BNFC.CastE (BNFC.EVar (BNFC.Var (BNFC.PIdent (p, id)))) e ->
      S.Cast pos (S.Custom (S.Ident p id)) (debloat e)
    
    BNFC.CastE e1 e2 -> error $ "parse error at " ++ show (position e1)

    where pos = position expr



instance ToBeDebloated BNFC.AddOp S.BinOp where
  debloat op = case op of
    BNFC.Plus _ -> S.Plus pos
    BNFC.Minus _ -> S.Minus pos

    where 
      pos = position op

instance ToBeDebloated BNFC.MulOp S.BinOp where
  debloat op = case op of
    BNFC.Times _ -> S.Times pos
    BNFC.Div _   -> S.Div pos
    BNFC.Mod _   -> S.Mod pos

    where 
      pos = position op


instance ToBeDebloated BNFC.RelOp S.RelOp where
  debloat op = case op of
    BNFC.LTH _ -> S.LTH pos
    BNFC.LE  _ -> S.LE  pos
    BNFC.GTH _ -> S.GTH pos
    BNFC.GE  _ -> S.GE  pos
    BNFC.EQU _ -> S.EQU pos
    BNFC.NE  _ -> S.NE  pos

    where
      pos = position op


instance ToBeDebloated BNFC.AndOp S.BoolOp where
  debloat op@(BNFC.And _) = S.And (position op)

instance ToBeDebloated BNFC.OrOp S.BoolOp where
  debloat op@(BNFC.Or _) = S.Or (position op)


instance ToBeDebloated BNFC.ClassBody S.ClassBody where
  debloat body@(BNFC.ClassBody _ memberDecls _)
    = S.ClassBody p1 p2 (map debloat memberDecls)

    where 
      p1 = position body
      p2 = endPosition body


instance ToBeDebloated BNFC.MemberDecl S.MemberDecl where
  debloat memberDecl = case memberDecl of
    BNFC.AttrDecl t attrId _ -> S.AttrDecl pos (debloat t) (debloat attrId)
    BNFC.MethodDecl def -> S.MethodDecl (debloat def)

  
    where pos = position memberDecl

-- Syntax to SyntaxGADT -------------------------------------------------------
-- only some elements can be easily "debloated"

instance ToBeDebloated S.Ident (DS.Ident a) where
  debloat (S.Ident pos x) = DS.Ident pos x

debloatScopedId :: Int -> S.Ident -> DS.ScopedIdent t
debloatScopedId lvl x = DS.Scoped lvl (debloat x)

instance ToBeDebloated S.Ident (DS.FuncIdent t ts) where
  debloat (S.Ident pos x) = DS.FuncIdent pos x

instance ToBeDebloated S.Ident (Some DS.ClassIdent) where
  debloat (S.Ident pos x) = case someFromString x of
    SomeSing s -> Some $ DS.ClassIdent pos s



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
      
    S.Custom cls -> case debloat cls of
      Some clsId -> Some $ DS.KWCustom clsId

bloatId :: DS.Ident a -> S.Ident
bloatId (DS.Ident p s) = S.Ident p s

