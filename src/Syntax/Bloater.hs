{-# LANGUAGE 
    MultiParamTypeClasses
  , GADTs
  , FlexibleInstances
#-}

module Syntax.Bloater where
    

import Data.Singletons.Sigma
import Data.Singletons.Prelude


import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import Position.Position (position, fakePos)

import Dependent
import SingChar


class ToBeBloated post pre where
  bloat :: post -> pre

instance ToBeBloated a b => ToBeBloated (Maybe a) (Maybe b) where
  bloat x = bloat <$> x



  

instance ToBeBloated (DS.Ident a) S.Ident where
  bloat (DS.Ident p id) = S.Ident p id

instance ToBeBloated (DS.ScopedIdent a) S.Ident where
  bloat (DS.Scoped _ id) = bloat id

instance ToBeBloated (DS.FuncIdent t ts) S.Ident where
  bloat (DS.FuncIdent p id) = S.Ident p id

instance ToBeBloated (DS.ClassIdent cls) S.Ident where
  bloat (DS.ClassIdent p id) = S.Ident p $ singToString id

instance ToBeBloated DS.Program S.Program where
  bloat (DS.Program p defs) = S.Program p (map bloat defs)

instance ToBeBloated (Either DS.ClassDef DS.FnDef) S.TopDef where
  bloat (Left def) = bloat def
  bloat (Right def) = bloat def

instance ToBeBloated DS.FnDef S.TopDef where
  bloat (DS.FnDef p t id params body)
    = S.FnDef p (bloat t) (bloat id) (bloat params) (bloat body)


instance ToBeBloated DS.ClassDef S.TopDef where
  bloat (DS.ClassDef p id parentId body)
    = S.ClassDef p (bloat id) debloatedParentId (bloat body)

    where
      debloatedParentId = case parentId of
        Nothing -> Nothing
        Just (_ :&: id) -> Just (bloat id)


instance ToBeBloated (DS.Param t) S.Param where
  bloat (DS.Param t id) = S.Param (bloat t) (bloat id)  

instance ToBeBloated (DS.ParamList ts) [S.Param] where 
  bloat DNil = []
  bloat (p :> ps) = bloat p : bloat ps



instance ToBeBloated DS.Block S.Block where
  bloat (DS.Block p stmts) = S.Block p (map bloat stmts)


instance ToBeBloated DS.Stmt S.Stmt where
  bloat stmt = case stmt of
    DS.Empty p                       -> S.Empty p
    DS.BStmt p block                 -> S.BStmt p (bloat block)
    DS.Decl p t items                -> S.Decl p (bloat t) (map bloat items)
    DS.Ass p _ var expr              -> S.Ass p (bloat var) (bloat expr)
    DS.Incr p var                    -> S.Incr p (bloat var)
    DS.Decr p var                    -> S.Decr p (bloat var)
    DS.Ret p _ expr                  -> S.Ret p (bloat expr)
    DS.VRet p                        -> S.VRet p
    DS.Cond p cond stm               -> S.Cond p (bloat cond) (bloat stm)
    DS.CondElse p cond stmIf stmElse -> S.CondElse p (bloat cond) (bloat stmIf) (bloat stmElse)
    DS.While p cond loopBody         -> S.While p (bloat cond) (bloat loopBody)
    DS.SExp p _ expr                 -> S.SExp p (bloat expr)
    DS.For p t id arr loopBody       -> S.For p (bloat t) (bloat id) (bloat arr) (bloat loopBody)
    


instance ToBeBloated (DS.Item a) S.Item where
  bloat (DS.NoInit id) = S.NoInit (bloat id)
  bloat (DS.Init id e) = S.Init (bloat id) (bloat e)





instance ToBeBloated (DS.TypeKW t) S.Type where

  bloat t = case t of
    DS.KWInt p        -> S.Int p
    DS.KWStr p        -> S.Str p
    DS.KWBool p       -> S.Bool p
    DS.KWVoid p       -> S.Void p
    DS.KWArr elemType -> S.Arr (bloat elemType)

    DS.KWCustom cls   -> S.Custom (bloat cls)

instance ToBeBloated DS.LatteType S.Type where

  bloat t = case t of
    DS.TInt         -> S.Int fakePos
    DS.TStr         -> S.Str fakePos
    DS.TBool        -> S.Bool fakePos
    DS.TVoid        -> S.Void fakePos
    DS.Arr elemType -> S.Arr (bloat elemType)

    DS.Custom cls   -> S.Custom $ S.Ident fakePos (toString cls)
    DS.TNull        -> error "INTERNAL ERROR (bloat NullT)"

instance ToBeBloated (DS.SLatteType t) S.Type where

  bloat t = bloat (fromSing t)
  

instance ToBeBloated (DS.Var a) S.Var where
  bloat var = case var of
    DS.Var    p id    -> S.Var    p (bloat id)
    DS.Attr   p e id  -> S.Member p (bloat e) (bloat id)
    DS.Elem   p e1 e2 -> S.Elem   p (bloat e1) (bloat e2)
    DS.Null   p       -> S.Null   p
    DS.Self   p       -> S.Self   p

instance ToBeBloated (DS.Callable t ts) S.Var where
  bloat var = case var of
    DS.Func     p id    -> S.Var    p (bloat id)
    DS.Method   p e id  -> S.Member p (bloat e) (bloat id)


instance ToBeBloated (DS.Expr a) S.Expr where
  bloat expr = case expr of
    DS.EVar     p _ var           -> S.EVar     p (bloat var)
    DS.ELitInt  p i               -> S.ELitInt  p (S.SInt p i)
    DS.ELitBool p b               -> S.ELitBool p b
    DS.EApp     p _ var _ args    -> S.EApp     p (bloat var) (bloatExprList args)
    DS.EString  p s               -> S.EString  p (S.SStr p s)
    DS.Neg      p e               -> S.Neg      p (bloat e)
    DS.Not      p e               -> S.Not      p (bloat e)
    DS.EOp      p op lhs rhs      -> S.EOp      p (bloat op) (bloat lhs) (bloat rhs)
    DS.ERel     p _ op lhs rhs    -> S.ERel     p (bloat op) (bloat lhs) (bloat rhs)
    DS.EBool    p op lhs rhs      -> S.EBool    p (bloat op) (bloat lhs) (bloat rhs)
    DS.NewArr   p t e             -> S.NewArr   p (bloat t) (bloat e)
    DS.NewObj   p (DS.KWCustom c) -> S.NewObj   p (bloat c)
    DS.Cast     p t e             -> S.Cast     p (bloat t) (bloat e)
    DS.Concat   p lhs rhs         -> S.EOp      p (S.Plus p) (bloat lhs) (bloat rhs)

    where
      bloatExprList :: DS.ExprList ts -> [S.Expr]
      bloatExprList DNil = []
      bloatExprList (e :> es) = bloat e : bloatExprList es



instance ToBeBloated DS.BinOp S.BinOp where
  bloat op = case op of
    DS.Plus   p -> S.Plus   p
    DS.Minus  p -> S.Minus  p
    DS.Times  p -> S.Times  p
    DS.Div    p -> S.Div    p
    DS.Mod    p -> S.Mod    p


instance ToBeBloated (DS.RelOp a) S.RelOp where
  bloat op = case op of
    DS.LTH p -> S.LTH p
    DS.LE  p -> S.LE  p
    DS.GTH p -> S.GTH p
    DS.GE  p -> S.GE  p
    DS.EQU p -> S.EQU p
    DS.NE  p -> S.NE  p
  

instance ToBeBloated DS.BoolOp S.BoolOp where
    bloat (DS.And p) = S.And p
    bloat (DS.Or  p) = S.Or  p



instance ToBeBloated DS.ClassBody S.ClassBody where
    bloat (DS.ClassBody p decls) = S.ClassBody p (map bloat decls)


instance ToBeBloated DS.MemberDecl S.MemberDecl where
  bloat decl = case decl of
    DS.AttrDecl p t id  -> S.AttrDecl p (bloat t) (bloat id)
    DS.MethodDecl def   -> S.MethodDecl (bloat def)
  


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
  bloat (S.SStr p s) = BNFC.PString (p, "\"" ++ s ++ "\"")

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
  



















