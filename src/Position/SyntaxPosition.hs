module Position.SyntaxPosition where

import Syntax.Syntax
import Position.Position (HasPosition, position)


  
instance HasPosition Ident where
  position (Ident p _) = p

instance HasPosition SInt where
  position (SInt p _) = p

instance HasPosition SStr where
  position (SStr p _) = p

instance HasPosition Program where
  position (Program p _) = p


instance HasPosition TopDef where
  position (FnDef p _ _ _ _) = p

  position (ClassDef p _ _ _) = p

instance HasPosition Param where
  position (Param t _) = position t

instance HasPosition Block where
    position (Block p _) = p

instance HasPosition Stmt where
  position stmt = case stmt of
    Empty     p         -> p
    BStmt     p _       -> p
    Decl      p _ _     -> p
    Ass       p _ _     -> p
    Incr      p _       -> p
    Decr      p _       -> p
    Ret       p _       -> p
    VRet      p         -> p
    Cond      p _ _     -> p
    CondElse  p _ _ _   -> p
    While     p _ _     -> p
    SExp      p _       -> p
    For       p _ _ _ _ -> p



instance HasPosition Item where
  position (NoInit id) = position id
  position (Init id _) = position id



instance HasPosition Type where
  position t = case t of
    Int   p   -> p
    Str   p   -> p
    Bool  p   -> p
    Void  p   -> p
    Arr   t   -> position t
    Custom id -> position id



instance HasPosition Var where
  position var = case var of
    Var     p _ -> p
    Fun     p _ -> p
    Member  p _ _ -> p
    Elem    p _ _ -> p
  


instance HasPosition Expr where
  position expr = case expr of
    EVar      p _     -> p
    ELitInt   p _     -> p
    ELitBool  p _     -> p
    EApp      p _ _   -> p
    EString   p _     -> p
    Neg       p _     -> p
    Not       p _     -> p
    EOp       p _ _ _ -> p
    ERel      p _ _ _ -> p
    EBool     p _ _ _ -> p
    NewArr    p _ _   -> p
    NewObj    p _     -> p
    Cast      p _ _   -> p




instance HasPosition BinOp where
    position (Plus  p) = p
    position (Minus p) = p
    position (Times p) = p
    position (Div   p) = p
    position (Mod   p) = p


instance HasPosition RelOp where
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
  




























