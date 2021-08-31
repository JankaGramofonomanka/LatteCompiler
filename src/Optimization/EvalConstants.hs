{-# LANGUAGE 
    GADTs
  , KindSignatures
  , DataKinds
  , TypeFamilies
  , FlexibleContexts
  , StandaloneKindSignatures
#-}

module Optimization.EvalConstants where


import Data.Kind

import Syntax.SyntaxDep
import Position.Position
import Position.SyntaxDepPosition

import Dependent




type Const :: LatteType -> Type
data Const t where
  IConst :: Int -> Const TInt
  BConst :: Bool -> Const TBool
  SConst :: String -> Const TStr

instance Eq (Const t) where
  IConst x == IConst y = x == y
  BConst x == BConst y = x == y
  SConst x == SConst y = x == y

instance Ord (Const t) where
  IConst x <= IConst y = x <= y
  BConst x <= BConst y = x <= y
  SConst x <= SConst y = x <= y

evalConstExpr :: Expr a -> Maybe (Const a)
evalConstExpr expr = case expr of
  EVar      _ _ var -> Nothing
  ELitInt   _ i     -> Just $ IConst i
  ELitBool  _ b     -> Just $ BConst b
  EApp      {}      -> Nothing
  EString   _ s     -> Just $ SConst s
  Neg       _ e -> do
    IConst val <- evalConstExpr e
    return $ IConst (-val)

  Not       _ e -> do
    BConst val <- evalConstExpr e
    return $ BConst $ not val

  EOp       _ op lhs rhs -> do
    IConst valLHS <- evalConstExpr lhs
    IConst valRHS <- evalConstExpr rhs
    let realOp = getBinOp op
    return $ IConst $ realOp valLHS valRHS

  ERel      _ _ op lhs rhs -> do
    valLHS <- evalConstExpr lhs
    valRHS <- evalConstExpr rhs
    let realOp = getRelOp op
    return $ BConst $ realOp valLHS valRHS

  EBool     _ op lhs rhs -> case op of
    And _ -> do
      BConst valLHS <- evalConstExpr lhs
      if not valLHS then
        return $ BConst False
      else do
        BConst valRHS <- evalConstExpr rhs
        return $ BConst $ valLHS && valRHS
    
    Or _ -> do
      BConst valLHS <- evalConstExpr lhs
      if valLHS then
        return $ BConst True
      else do
        BConst valRHS <- evalConstExpr rhs
        return $ BConst $ valLHS || valRHS

  NewArr    {} -> Nothing
  NewObj    {} -> Nothing
  Cast      {} -> Nothing

  Concat    _ lhs rhs -> do
    SConst valLHS <- evalConstExpr lhs
    SConst valRHS <- evalConstExpr rhs
    return $ SConst $ valLHS ++ valRHS




getBinOp :: BinOp -> (Int -> Int -> Int)
getBinOp (Plus  _) = (+)
getBinOp (Minus _) = (-)
getBinOp (Times _) = (*)
getBinOp (Div   _) = div
getBinOp (Mod   _) = rem

getRelOp :: RelOp a -> (Const a -> Const a -> Bool)
getRelOp op = case op of
  LTH _ -> (<)
  LE  _ -> (<=)
  GTH _ -> (>)
  GE  _ -> (>=)
  EQU _ -> (==)
  NE  _ -> (/=)


getBoolOp :: BoolOp -> (Bool -> Bool -> Bool)
getBoolOp (And  _) = (&&)
getBoolOp (Or   _) = (||)




class MayHaveConstants a where
  evalConstants :: a -> a

mkLit :: Pos -> Const a -> Expr a
mkLit p (IConst i) = ELitInt p i
mkLit p (BConst b) = ELitBool p b
mkLit p (SConst s) = EString p s

zipExpr :: Expr a -> Expr a
zipExpr e = case evalConstExpr e of
  Just c -> mkLit (position e) c
  Nothing -> evalConstants e

instance MayHaveConstants (Expr a) where
  evalConstants expr = case expr of
    EVar      _ _ var -> expr
    ELitInt   _ i     -> expr
    ELitBool  _ b     -> expr
    EApp      p t f argTs args -> EApp p t f argTs (evalConstants' args)
      where
        evalConstants' :: ExprList ts -> ExprList ts
        evalConstants' DNil = DNil
        evalConstants' (arg :> args) = evalConstants arg :> evalConstants' args

    EString   _ s     -> expr
    Neg       p e -> case evalConstExpr e of
      Just (IConst c) -> ELitInt p (-c)
      Nothing -> Neg p $ evalConstants e

    Not       p e -> case evalConstExpr e of
      Just (BConst c) -> ELitBool p (not c)
      Nothing -> Not p $ evalConstants e

    EOp       p op lhs rhs -> case evalConstExpr expr of
      Just c -> mkLit p c
      Nothing -> EOp p op (zipExpr lhs) (zipExpr rhs)

    ERel      p t op lhs rhs -> case evalConstExpr expr of
      Just c -> mkLit p c
      Nothing -> ERel p t op (zipExpr lhs) (zipExpr rhs)

    EBool     p op lhs rhs -> case evalConstExpr expr of
      Just c -> mkLit p c
      Nothing -> EBool p op (zipExpr lhs) (zipExpr rhs)

    NewArr    p t n -> NewArr p t (evalConstants n)
    NewObj    {} -> expr
    Cast      p t e -> Cast p t (evalConstants e)

    Concat    p lhs rhs -> case evalConstExpr expr of
      Just c -> mkLit p c
      Nothing -> Concat p (zipExpr lhs) (zipExpr rhs)


instance MayHaveConstants Block where
  evalConstants (Block p stmts) = Block p (map evalConstants stmts)

instance MayHaveConstants (Item t) where
  evalConstants item = case item of
    NoInit x -> item
    Init x e -> Init x (evalConstants e)

instance MayHaveConstants Stmt where
  evalConstants stmt = case stmt of
    Empty p -> Empty p
    BStmt p block -> BStmt p (evalConstants block)
    
    Decl p t items -> Decl p t (map evalConstants items)
    Ass p t var expr -> Ass p t var (evalConstants expr)
    
    Incr p var -> stmt
    Decr p var -> stmt
    
    Ret p t expr -> Ret p t (evalConstants expr)
    VRet p -> VRet p
    
    Cond p cond stm -> case evalConstExpr cond of
      Nothing -> Cond p (evalConstants cond) (evalConstants stm)
      Just (BConst True) -> stm
      Just (BConst False) -> Empty p

    CondElse p cond stmIf stmElse -> case evalConstExpr cond of
      Nothing -> CondElse p
        (evalConstants cond)
        (evalConstants stmIf)
        (evalConstants stmElse)
      
      Just (BConst True) -> stmIf
      Just (BConst False) -> stmElse

    While p cond loopBody -> case evalConstExpr cond of
      Just (BConst True)  -> Forever p (evalConstants loopBody)
      Just (BConst False) -> Empty p
      
      _ -> While p (evalConstants cond) (evalConstants loopBody)

    SExp p t expr -> SExp p t (evalConstants expr)

    -- TODO `arr` may have be turned into an expression and need evaluation
    For p t id arr loopBody -> 
      For p t id arr (evalConstants loopBody)
    
    Forever p stmt -> Forever p (evalConstants stmt)


instance MayHaveConstants Program where
  evalConstants (Program p defs) = Program p (map evalConstants' defs)
    where
      evalConstants' (Left def) = Left (evalConstants def)
      evalConstants' (Right def) = Right (evalConstants def)

instance MayHaveConstants FnDef where
  evalConstants (FnDef p retT funcId params body)
    = FnDef p retT funcId params (evalConstants body)

instance MayHaveConstants ClassDef where
  evalConstants (ClassDef p clsId parent body)
    = ClassDef p clsId parent (evalConstants body)




instance MayHaveConstants ClassBody where
  evalConstants (ClassBody p memberDecls)
    = ClassBody p (map evalConstants memberDecls)


instance MayHaveConstants MemberDecl where
  evalConstants decl = case decl of
    AttrDecl p t x -> decl
    MethodDecl def -> MethodDecl (evalConstants def)
  



