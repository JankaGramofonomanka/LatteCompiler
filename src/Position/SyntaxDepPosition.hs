{-# LANGUAGE GADTs #-}

module Position.SyntaxDepPosition where

import Syntax.SyntaxDep
import Position.Position



  
instance HasPosition (Ident t) where
  position (Ident p _) = p

instance HasPosition (ScopedIdent t) where
  position (Scoped _ x) = position x

instance HasPosition (FuncIdent t ts) where
  position (FuncIdent p _) = p

instance HasPosition (ClassIdent cls) where
  position (ClassIdent p _) = p

instance HasPosition Program where
  position (Program p _) = p


instance HasPosition FnDef where
  position (FnDef p _ _ _ _) = p

instance HasPosition ClassDef where
  position (ClassDef p _ _ _) = p


instance HasPosition Block where
    position (Block p _) = p

instance HasPosition Stmt where
  position stmt = case stmt of
    Empty     p         -> p
    BStmt     p _       -> p
    Decl      p _ _     -> p
    Ass       p _ _ _   -> p
    Incr      p _       -> p
    Decr      p _       -> p
    Ret       p _ _     -> p
    VRet      p         -> p
    Cond      p _ _     -> p
    CondElse  p _ _ _   -> p
    While     p _ _     -> p
    SExp      p _ _     -> p
    For       p _ _ _ _ -> p
    Forever   p _       -> p



instance HasPosition (Item t) where
  position (NoInit id) = position id
  position (Init id _) = position id



instance HasPosition (TypeKW t) where
  position t = case t of
    KWInt p       -> p
    KWStr p       -> p
    KWBool p      -> p
    KWVoid p      -> p
    KWArr elemT   -> position elemT
    KWCustom cls  -> position cls

instance HasPosition (Var t) where
  position var = case var of
    Var   p _     -> p
    Attr  p _ _ _ -> p
    Length p _    -> p
    Elem  p _ _   -> p
    Null  p       -> p
    Self  p       -> p
  


instance HasPosition (Expr t) where
  position expr = case expr of
    EVar      p _ _     -> p
    ELitInt   p _       -> p
    ELitBool  p _       -> p
    EApp      p _ _ _ _ -> p
    EString   p _       -> p
    Neg       p _       -> p
    Not       p _       -> p
    EOp       p _ _ _   -> p
    ERel      p _ _ _ _ -> p
    EBool     p _ _ _   -> p
    NewArr    p _ _     -> p
    NewObj    p _       -> p
    Cast      p _ _     -> p
    Concat    p _ _     -> p




instance HasPosition BinOp where
  position (Plus  p) = p
  position (Minus p) = p
  position (Times p) = p
  position (Div   p) = p
  position (Mod   p) = p

instance HasPosition (RelOp a) where
  position (LTH p) = p
  position (LE  p) = p
  position (GTH p) = p
  position (GE  p) = p
  position (EQU p) = p
  position (NE  p) = p

instance HasPosition BoolOp where
  position (And p) = p
  position (Or  p) = p

    



instance HasPosition ClassBody where
    position (ClassBody p _) = p


instance HasPosition MemberDecl where
  position (AttrDecl p _ _) = p
  position (MethodDecl def) = position def
  




























