{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Syntax.Bloater where
    


import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxGADT as GS
import Position.Position (position, fakePos)



class ToBeBloated post pre where
  bloat :: post -> pre

instance ToBeBloated a b => ToBeBloated (Maybe a) (Maybe b) where
  bloat x = bloat <$> x



instance ToBeBloated (GS.Ident a) S.Ident where
  bloat (GS.Ident p id) = S.Ident p id

instance ToBeBloated GS.SInt S.SInt where
  bloat (GS.SInt p i) = S.SInt p i

instance ToBeBloated GS.SStr S.SStr where
  bloat (GS.SStr p s) = S.SStr p s

instance ToBeBloated GS.Program S.Program where
  bloat (GS.Program p defs) = S.Program p (map bloat defs)

instance ToBeBloated GS.TopDef S.TopDef where
  bloat (GS.FnDef p t id params body)
    = S.FnDef p (bloat t) (bloat id) (map bloat params) (bloat body)

  bloat (GS.ClassDef p id parentId body)
    = S.ClassDef p (bloat id) debloatedParentId (bloat body)

    where
      debloatedParentId = case parentId of
        Nothing -> Nothing
        Just id -> Just (bloat id)


instance ToBeBloated GS.Param S.Param where 
  bloat (GS.Param t id) = S.Param (bloat t) (bloat id)


instance ToBeBloated GS.Block S.Block where
  bloat (GS.Block p stmts) = S.Block p (map bloat stmts)

instance ToBeBloated GS.Stmt S.Stmt where
  bloat stmt = case stmt of
    GS.Empty p                       -> S.Empty p
    GS.BStmt p block                 -> S.BStmt p (bloat block)
    GS.Decl p t items                -> S.Decl p (bloat t) (map bloat items)
    GS.Ass p var expr                -> S.Ass p (bloat var) (bloat expr)
    GS.Incr p var                    -> S.Incr p (bloat var)
    GS.Decr p var                    -> S.Decr p (bloat var)
    GS.Ret p expr                    -> S.Ret p (bloat expr)
    GS.VRet p                        -> S.VRet p
    GS.Cond p cond stm               -> S.Cond p (bloat cond) (bloat stm)
    GS.CondElse p cond stmIf stmElse -> S.CondElse p (bloat cond) (bloat stmIf) (bloat stmElse)
    GS.While p cond loopBody         -> S.While p (bloat cond) (bloat loopBody)
    GS.SExp p expr                   -> S.SExp p (bloat expr)
    GS.For p t id arr loopBody       -> S.For p (bloat t) (bloat id) (bloat arr) (bloat loopBody)
    


instance ToBeBloated (GS.Item a) S.Item where
  bloat (GS.NoInit id) = S.NoInit (bloat id)
  bloat (GS.Init id e) = S.Init (bloat id) (bloat e)



instance ToBeBloated (GS.Type a) S.Type where

  bloat t = case t of
    GS.Int p        -> S.Int p
    GS.Str p        -> S.Str p
    GS.Bool p       -> S.Bool p
    GS.Void p       -> S.Void p
    GS.Arr elemType -> S.Arr (bloat elemType)

    GS.Custom (GS.Ident p id) -> S.Custom (S.Ident p id)
    
    GS.NullT -> error "INTERNAL ERROR (bloat NullT)"

instance ToBeBloated GS.AnyType S.Type where
  bloat (GS.AnyT t) = bloat t

instance ToBeBloated (GS.Var a) S.Var where
  bloat var = case var of
    GS.Var    p id    -> S.Var    p (bloat id)
    GS.Member p e id  -> S.Member p (bloat e) (bloat id)
    GS.Elem   p e1 e2 -> S.Elem   p (bloat e1) (bloat e2)
    GS.Null   p       -> S.Null   p
    GS.Self   p       -> S.Self   p


instance ToBeBloated (GS.Expr a) S.Expr where
  bloat expr = case expr of
    GS.EVar     p var             -> S.EVar     p (bloat var)
    GS.ELitInt  p i               -> S.ELitInt  p (bloat i)
    GS.ELitBool p b               -> S.ELitBool p b
    GS.EApp     p var args        -> S.EApp     p (bloat var) (map bloatAny args)
    GS.EString  p s               -> S.EString  p (bloat s)
    GS.Neg      p e               -> S.Neg      p (bloat e)
    GS.Not      p e               -> S.Not      p (bloat e)
    GS.EOp      p op lhs rhs      -> S.EOp      p (bloat op) (bloat lhs) (bloat rhs)
    GS.ERel     p op lhs rhs      -> S.ERel     p (bloat op) (bloat lhs) (bloat rhs)
    GS.EBool    p op lhs rhs      -> S.EBool    p (bloat op) (bloat lhs) (bloat rhs)
    GS.NewArr   p t e             -> S.NewArr   p (bloat t) (bloat e)
    GS.NewObj   p (GS.Custom cls) -> S.NewObj   p (bloat cls)
    GS.Cast     p t e             -> S.Cast     p (bloat t) (bloat e)
    GS.Concat   p lhs rhs         -> S.EOp      p (S.Plus p) (bloat lhs) (bloat rhs)

    where
      bloatAny :: GS.Any GS.Expr -> S.Expr
      bloatAny (GS.Any _ expr) = bloat expr


instance ToBeBloated GS.BinOp S.BinOp where
  bloat op = case op of
    GS.Plus   p -> S.Plus   p
    GS.Minus  p -> S.Minus  p
    GS.Times  p -> S.Times  p
    GS.Div    p -> S.Div    p
    GS.Mod    p -> S.Mod    p


instance ToBeBloated (GS.RelOp a) S.RelOp where
  bloat op = case op of
    GS.LTH p -> S.LTH p
    GS.LE  p -> S.LE  p
    GS.GTH p -> S.GTH p
    GS.GE  p -> S.GE  p
    GS.EQU p -> S.EQU p
    GS.NE  p -> S.NE  p
  

instance ToBeBloated GS.BoolOp S.BoolOp where
    bloat (GS.And p) = S.And p
    bloat (GS.Or  p) = S.Or  p



instance ToBeBloated GS.ClassBody S.ClassBody where
    bloat (GS.ClassBody p decls) = S.ClassBody p (map bloat decls)


instance ToBeBloated GS.MemberDecl S.MemberDecl where
  bloat decl = case decl of
    GS.AttrDecl p t id  -> S.AttrDecl p (bloat t) (bloat id)
    GS.MethodDecl def   -> S.MethodDecl (bloat def)
  


-------------------------------------------------------------------------------


lBrace = BNFC.PLBrace (fakePos, "{")
rBrace = BNFC.PRBrace (fakePos, "}")

classKW = BNFC.PClass (fakePos, "class")
extendsKW = BNFC.PExtends (fakePos, "exptends")

sep     = BNFC.PSemiColon (fakePos, ";")
retKW   = BNFC.PReturn  (fakePos, "return")
ifKW    = BNFC.PIf      (fakePos, "if")
elseKW  = BNFC.PElse    (fakePos, "else")
whileKW = BNFC.PWhile   (fakePos, "while")
forKW   = BNFC.PFor     (fakePos, "for")


plusKW  = BNFC.PPlus   (fakePos, "+")
minusKW = BNFC.PMinus  (fakePos, "-")
timesKW = BNFC.PTimes  (fakePos, "*")
divKW   = BNFC.PDiv    (fakePos, "/")
modKW   = BNFC.PMod    (fakePos, "%")

notKW = BNFC.PNot (fakePos, "-")

andKW = BNFC.PAnd (fakePos, "&&")
orKW = BNFC.POr (fakePos, "||")


newKW = BNFC.PNew (fakePos, "new")
nullKW = BNFC.PNull (fakePos, "null")
selfKW = BNFC.PSelf (fakePos, "self")


intKW = BNFC.PTypeInt (fakePos, "int")
strKW = BNFC.PTypeStr (fakePos, "string")
boolKW = BNFC.PTypeBool (fakePos, "boolean")
voidKW = BNFC.PTypeVoid (fakePos, "void")


instance ToBeBloated S.Ident BNFC.PIdent where
  bloat (S.Ident p id) = BNFC.PIdent (p, id)

instance ToBeBloated S.SInt BNFC.PInteger where
  bloat (S.SInt p i) = BNFC.PInteger (p, show i)

instance ToBeBloated S.SStr BNFC.PString where
  bloat (S.SStr p s) = BNFC.PString (p, s)

instance ToBeBloated S.Program BNFC.Program where
  bloat (S.Program p defs) = BNFC.Program $ map bloat defs


instance ToBeBloated S.TopDef BNFC.TopDef where
  bloat (S.FnDef p t id params body)
    = BNFC.FnDef (bloat t) (bloat id) (map bloat params) (bloat body)

  bloat (S.ClassDef p id Nothing body)
    = BNFC.BaseClassDef classKW (bloat id) (bloat body)

  bloat (S.ClassDef p id (Just parentId) body)
    = BNFC.ChildClassDef classKW (bloat id) extendsKW (bloat parentId) (bloat body)


  


instance ToBeBloated S.Param BNFC.Param where
  bloat (S.Param t id) = BNFC.Param (bloat t) (bloat id)

instance ToBeBloated S.Block BNFC.Block where
  bloat (S.Block p stmts) = BNFC.Block lBrace (map bloat stmts) rBrace


instance ToBeBloated S.Stmt BNFC.Stmt where
  bloat stmt = case stmt of
    S.Empty     p             -> BNFC.Empty     sep
    S.BStmt     p block       -> BNFC.BStmt     (bloat block)
    S.Decl      p t items     -> BNFC.Decl      (bloat t) (map bloat items) sep
    S.Ass       p v e         -> BNFC.Ass       (bloat v) (bloat e) sep
    S.Incr      p v           -> BNFC.Incr      (bloat v) sep
    S.Decr      p v           -> BNFC.Decr      (bloat v) sep
    S.Ret       p e           -> BNFC.Ret       retKW (bloat e) sep
    S.VRet      p             -> BNFC.VRet      retKW sep
    S.Cond      p e stm       -> BNFC.Cond      ifKW (bloat e) (bloat stm)
    S.CondElse  p e stm1 stm2 -> 
        BNFC.CondElse  ifKW (bloat e) (bloat stm1) elseKW (bloat stm2)

    S.While     p e stm       -> BNFC.While     whileKW (bloat e) (bloat stm)
    S.SExp      p e           -> BNFC.SExp      (bloat e) sep
    S.For       p t id v stm  -> 
        BNFC.For       forKW (bloat t) (bloat id) (bloat v) (bloat stm)



instance ToBeBloated S.Item BNFC.Item where
  bloat (S.NoInit id) = BNFC.NoInit (bloat id)
  bloat (S.Init id p) = BNFC.Init (bloat id) (bloat p)



instance ToBeBloated S.Type BNFC.Type where
  bloat t = case t of
    S.Int  p    -> BNFC.Int  intKW
    S.Str  p    -> BNFC.Str  strKW
    S.Bool p    -> BNFC.Bool boolKW
    S.Void p    -> BNFC.Void voidKW
    S.Arr elemT -> BNFC.Arr (bloat elemT)
    S.Custom id -> BNFC.Custom (bloat id)




instance ToBeBloated S.Var BNFC.Var where
  bloat var = case var of
    S.Var    p id     -> BNFC.Var     (bloat id)
    S.Member p e id   -> BNFC.Member  (bloat e) (bloat id)
    S.Elem   p e1 e2  -> BNFC.Elem    (bloat e1) (bloat e2 )
    S.Null   p        -> BNFC.Null    nullKW
    S.Self   p        -> BNFC.Self    selfKW
  


instance ToBeBloated S.Expr BNFC.Expr where
  bloat expr = case expr of
    S.EVar      p var             -> BNFC.EVar       (bloat var)
    S.ELitInt   p i               -> BNFC.ELitInt    (bloat i)
    S.ELitBool  p True            -> BNFC.ELitTrue   $ BNFC.PTrue (p, "true")
    S.ELitBool  p False           -> BNFC.ELitFalse  $ BNFC.PFalse (p, "false")
    S.EApp      p var args        -> BNFC.EApp       (bloat var) (map bloat args)
    S.EString   p s               -> BNFC.EString    (bloat s)
    S.Neg       p e               -> BNFC.Neg        minusKW (bloat e)
    S.Not       p e               -> BNFC.Not        notKW (bloat e)
    S.EOp       p op l r          -> case op of
      S.Plus  p -> BNFC.EAdd (bloat l) plus   (bloat r)
      S.Minus p -> BNFC.EAdd (bloat l) minus  (bloat r)
      S.Times p -> BNFC.EMul (bloat l) times  (bloat r)
      S.Div   p -> BNFC.EMul (bloat l) div    (bloat r)
      S.Mod   p -> BNFC.EMul (bloat l) mod    (bloat r)
      
    S.ERel      p op l r          -> BNFC.ERel       (bloat l) (bloat op) (bloat r)
    S.EBool     p (S.And _) l r   -> BNFC.EAnd       (bloat l) and (bloat r)
    S.EBool     p (S.Or _) l r    -> BNFC.EOr        (bloat l) or (bloat r)
    S.NewArr    p t e             -> BNFC.NewArr     newKW (bloat t) (bloat e)
    S.NewObj    p id              -> BNFC.NewObj     newKW (bloat id)
    S.Cast      p t e             -> BNFC.Cast       (bloat t) (bloat e)

    where

      plus  = BNFC.Plus  plusKW
      minus = BNFC.Minus minusKW
      times = BNFC.Times timesKW
      div   = BNFC.Div   divKW
      mod   = BNFC.Mod   modKW

      and = BNFC.And andKW
      or = BNFC.Or orKW


instance ToBeBloated S.BinOp BNFC.AddOp where
  bloat (S.Plus p)  = BNFC.Plus plusKW
  bloat (S.Minus p) = BNFC.Minus minusKW
  bloat _           = undefined

instance ToBeBloated S.BinOp BNFC.MulOp where
  bloat (S.Times p) = BNFC.Times timesKW
  bloat (S.Div p)   = BNFC.Div   divKW
  bloat (S.Mod p)   = BNFC.Mod   modKW
  bloat _           = undefined


instance ToBeBloated S.RelOp BNFC.RelOp where
  bloat op = case op of
    S.LTH p -> BNFC.LTH (BNFC.PLTH (p, "<"))
    S.LE  p -> BNFC.LE  (BNFC.PLE  (p, "<="))
    S.GTH p -> BNFC.GTH (BNFC.PGTH (p, ">"))
    S.GE  p -> BNFC.GE  (BNFC.PGE  (p, ">="))
    S.EQU p -> BNFC.EQU (BNFC.PEQU (p, "="))
    S.NE  p -> BNFC.NE  (BNFC.PNE  (p, "!="))

instance ToBeBloated S.BoolOp BNFC.AndOp where
  bloat (S.And p) = BNFC.And andKW
  bloat _         = undefined
  

instance ToBeBloated S.BoolOp BNFC.OrOp where
  bloat (S.Or p)  = BNFC.Or orKW
  bloat _         = undefined




instance ToBeBloated S.ClassBody BNFC.ClassBody where
  bloat (S.ClassBody p decls) = BNFC.ClassBody lBrace (map bloat decls) rBrace


instance ToBeBloated S.MemberDecl BNFC.MemberDecl where
  bloat (S.AttrDecl p t id) = BNFC.AttrDecl (bloat t) (bloat id) sep
  bloat (S.MethodDecl def) = BNFC.MethodDecl (bloat def)
  



















