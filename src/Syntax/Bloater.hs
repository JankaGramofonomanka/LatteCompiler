{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Syntax.Bloater where
    


import qualified FromBNFC.AbsLatte as Bloated
import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxGADT as GS
import Position.Position (position)



class ToBeBloated post pre where
  bloat :: post -> pre




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
  






