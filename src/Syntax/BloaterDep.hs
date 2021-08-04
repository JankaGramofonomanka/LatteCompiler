{-# LANGUAGE 
    MultiParamTypeClasses
  , GADTs
  , FlexibleInstances
#-}

module Syntax.BloaterDep where
    
import qualified Data.Map as M
import Data.Singletons.Sigma
import Data.Singletons.Prelude

import Syntax.Bloater
import qualified FromBNFC.AbsLatte as BNFC
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import Position.Position (position, fakePos)
import Dependent




instance ToBeBloated (DS.Ident a) S.Ident where
  bloat (DS.Ident p id) = S.Ident p id

instance ToBeBloated (DS.FuncIdent t ts) S.Ident where
  bloat (DS.FuncIdent p id) = S.Ident p id

instance ToBeBloated (DS.ClassIdent cls) S.Ident where
  bloat (DS.ClassIdent p id) = S.Ident p id

--instance ToBeBloated DS.Program S.Program where
--  bloat (DS.Program p defs) = S.Program p (map bloat defs)

{-
instance ToBeBloated DS.FnDef S.TopDef where
  bloat (DS.FnDef p t id params body)
    = S.FnDef p (bloat t) (bloat id) (bloatParams params) (bloat body)

    where
      bloatParams :: DS.ParamList ts -> [S.Param]
      bloatParams DNil = []
      bloatParams (x@(DS.Ident p s) :> ps) = let
        (S.Param ) : bloatParams ps

instance ToBeBloated DS.ClassDef S.TopDef where
  bloat (DS.ClassDef p id parentId body)
    = S.ClassDef p (bloat id) debloatedParentId (bloat body)

    where
      debloatedParentId = case parentId of
        Nothing -> Nothing
        Just (_ :&: id) -> Just (bloat id)
-- -}

instance ToBeBloated DS.Block S.Block where
  bloat (DS.Block p stmts) = S.Block p (map bloat stmts)


instance ToBeBloated DS.Stmt S.Stmt where
  bloat stmt = case stmt of
    DS.Empty p                       -> S.Empty p
    DS.BStmt p block                 -> S.BStmt p (bloat block)
    DS.Decl p t items                -> S.Decl p (bloat t) (map bloat items)
    DS.Ass p var expr                -> S.Ass p (bloat var) (bloat expr)
    DS.Incr p var                    -> S.Incr p (bloat var)
    DS.Decr p var                    -> S.Decr p (bloat var)
    DS.Ret p expr                    -> S.Ret p (bloat expr)
    DS.VRet p                        -> S.VRet p
    DS.Cond p cond stm               -> S.Cond p (bloat cond) (bloat stm)
    DS.CondElse p cond stmIf stmElse -> S.CondElse p (bloat cond) (bloat stmIf) (bloat stmElse)
    DS.While p cond loopBody         -> S.While p (bloat cond) (bloat loopBody)
    DS.SExp p expr                   -> S.SExp p (bloat expr)
    DS.For p t id arr loopBody       -> S.For p (bloat t) (bloat id) (bloat arr) (bloat loopBody)
    


instance ToBeBloated (DS.Item a) S.Item where
  bloat (DS.NoInit id) = S.NoInit (bloat id)
  bloat (DS.Init id e) = S.Init (bloat id) (bloat e)



--instance ToBeBloated (DS.SLatteType t) S.Type where
--  bloat st = bloat (fromSing st)


instance ToBeBloated (DS.TypeKW t) S.Type where

  bloat t = case t of
    DS.KWInt p        -> S.Int fakePos
    DS.KWStr p        -> S.Str fakePos
    DS.KWBool p       -> S.Bool fakePos
    DS.KWVoid p       -> S.Void fakePos
    DS.KWArr elemType -> S.Arr (bloat elemType)

    DS.KWCustom cls   -> S.Custom (bloat cls)


--instance ToBeBloated (Some DS.SLatteType) S.Type where
--  bloat (Some t) = bloat t

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
    DS.EVar     p var             -> S.EVar     p (bloat var)
    DS.ELitInt  p i               -> S.ELitInt  p (S.SInt p i)
    DS.ELitBool p b               -> S.ELitBool p b
    DS.EApp     p var args        -> S.EApp     p (bloat var) (bloatExprList args)
    DS.EString  p s               -> S.EString  p (S.SStr p s)
    DS.Neg      p e               -> S.Neg      p (bloat e)
    DS.Not      p e               -> S.Not      p (bloat e)
    DS.EOp      p op lhs rhs      -> S.EOp      p (bloat op) (bloat lhs) (bloat rhs)
    DS.ERel     p op lhs rhs      -> S.ERel     p (bloat op) (bloat lhs) (bloat rhs)
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



--instance ToBeBloated DS.ClassBody S.ClassBody where
--    bloat (DS.ClassBody p decls) = S.ClassBody p (map bloat decls)


--instance ToBeBloated DS.MemberDecl S.MemberDecl where
--  bloat decl = case decl of
--    DS.AttrDecl p t id  -> S.AttrDecl p (bloat t) (bloat id)
--    DS.MethodDecl def   -> S.MethodDecl (bloat def)
  





















