module Position.Position where

import FromBNFC.AbsLatte


-- {-
type Pos = (Int, Int)
fakePos, zeroPos :: Pos
fakePos = (-1, -1)
zeroPos = (0, 0)


class HasPosition a where
  position :: a -> Pos

instance HasPosition PTrue where
  position (PTrue (p, _)) = p

instance HasPosition PFalse where
  position (PFalse (p, _)) = p

instance HasPosition PTypeInt where
  position (PTypeInt (p, _)) = p

instance HasPosition PTypeStr where
  position (PTypeStr (p, _)) = p

instance HasPosition PTypeBool where
  position (PTypeBool (p, _)) = p

instance HasPosition PTypeVoid where
  position (PTypeVoid (p, _)) = p

instance HasPosition PPlus where
  position (PPlus (p, _)) = p

instance HasPosition PMinus where
  position (PMinus (p, _)) = p

instance HasPosition PTimes where
  position (PTimes (p, _)) = p

instance HasPosition PDiv where
  position (PDiv (p, _)) = p

instance HasPosition PMod where
  position (PMod (p, _)) = p

instance HasPosition PLTH where
  position (PLTH (p, _)) = p

instance HasPosition PLE where
  position (PLE (p, _)) = p

instance HasPosition PGTH where
  position (PGTH (p, _)) = p

instance HasPosition PGE where
  position (PGE (p, _)) = p

instance HasPosition PEQU where
  position (PEQU (p, _)) = p

instance HasPosition PNE where
  position (PNE (p, _)) = p

instance HasPosition PAnd where
  position (PAnd (p, _)) = p

instance HasPosition POr where
  position (POr (p, _)) = p

instance HasPosition PNot where
  position (PNot (p, _)) = p

instance HasPosition PLBrace where
  position (PLBrace (p, _)) = p

instance HasPosition PRBrace where
  position (PRBrace (p, _)) = p

instance HasPosition PSemiColon where
  position (PSemiColon (p, _)) = p

instance HasPosition PIf where
  position (PIf (p, _)) = p

instance HasPosition PElse where
  position (PElse (p, _)) = p

instance HasPosition PWhile where
  position (PWhile (p, _)) = p

instance HasPosition PFor where
  position (PFor (p, _)) = p

instance HasPosition PReturn where
  position (PReturn (p, _)) = p

instance HasPosition PNew where
  position (PNew (p, _)) = p

instance HasPosition PClass where
  position (PClass (p, _)) = p

instance HasPosition PExtends where
  position (PExtends (p, _)) = p

instance HasPosition PNull where 
  position (PNull (p, _)) = p

instance HasPosition PSelf where
  position (PSelf (p, _)) = p

instance HasPosition PIdent where
  position (PIdent (p, _)) = p

instance HasPosition PInteger where
  position (PInteger (p, _)) = p

instance HasPosition PString where
  position (PString (p, _)) = p

instance HasPosition Program where
  position (Program _) = zeroPos


instance HasPosition TopDef where
  position def = case def of
    FnDef t _ _ _             -> position t
    BaseClassDef kw _ _       -> position kw
    ChildClassDef kw _ _ _ _  -> position kw


instance HasPosition Param where
  position (Param t _) = position t


instance HasPosition Block where
  position (Block leftBrace _ _) = position leftBrace


instance HasPosition Stmt where
  position stmt = case stmt of
    Empty semiColon     -> position semiColon
    BStmt block         -> position block
    Decl t _ _          -> position t
    Ass var _ _         -> position var
    Incr var _          -> position var
    Decr var _          -> position var
    Ret kw _ _          -> position kw
    VRet kw _           -> position kw
    Cond kw _ _         -> position kw
    CondElse kw _ _ _ _ -> position kw
    While kw _ _        -> position kw
    SExp expr _         -> position expr
    For kw _ _ _ _      -> position kw


instance HasPosition Item where
  position (NoInit id) = position id
  position (Init id _) = position id


instance HasPosition Type where
  position t = case t of
    Int kw    -> position kw
    Str kw    -> position kw
    Bool kw   -> position kw
    Void kw   -> position kw
    Arr tt    -> position tt
    Custom id -> position id


instance HasPosition Var where
  position (Var id)       = position id
  position (Member var _) = position var
  position (Elem var _)   = position var
  position (Null kw)      = position kw
  position (Self kw)      = position kw


instance HasPosition Expr where
  position expr = case expr of
    EVar var      -> position var
    ELitInt lit   -> position lit
    ELitTrue lit  -> position lit
    ELitFalse lit -> position lit
    EApp var _    -> position var
    EString lit   -> position lit
    Neg kw _      -> position kw
    Not kw _      -> position kw
    EMul e1 _ _   -> position e1
    EAdd e1 _ _   -> position e1
    ERel e1 _ _   -> position e1
    EAnd e1 _ _   -> position e1
    EOr e1 _ _    -> position e1
    NewArr kw _ _ -> position kw
    NewObj kw _   -> position kw
    Cast t _      -> position t
    CastE e _     -> position e


instance HasPosition AddOp where
  position (Plus kw)  = position kw
  position (Minus kw) = position kw


instance HasPosition MulOp where
  position (Times kw) = position kw
  position (Div kw)   = position kw
  position (Mod kw)   = position kw


instance HasPosition RelOp where
  position op = case op of
    LTH kw -> position kw
    LE  kw -> position kw
    GTH kw -> position kw
    GE  kw -> position kw
    EQU kw -> position kw
    NE  kw -> position kw


instance HasPosition AndOp where
  position (And kw) = position kw


instance HasPosition OrOp where
  position (Or kw) = position kw


instance HasPosition ClassBody where
  position (ClassBody leftBrace _ _) = position leftBrace


instance HasPosition MemberDecl where
  position (AttrDecl t _ _) = position t
  position (MethodDecl def) = position def


