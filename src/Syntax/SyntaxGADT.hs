{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.SyntaxGADT where
import Position.Position (Pos)



newtype Void = Vd ()
newtype Custom = Cst ()
newtype Func = Func ()


newtype Class = Class ()
newtype Array a = Array ()

data Any (a :: * -> *) where
  Any :: Type b -> a b -> Any a


data AnyType where
  AnyT :: Type a -> AnyType


  
data Ident a = Ident Pos String deriving (Ord, Show, Read)
type FuncIdent = Ident Func
type ClassIdent = Ident Class

instance Eq (Ident a) where
  Ident _ x == Ident _ y = x == y


data SInt = SInt Pos Int deriving (Ord, Show, Read)

instance Eq SInt where
  SInt _ i == SInt _ j = i == j

data SStr = SStr Pos String deriving (Ord, Show, Read)

instance Eq SStr where
  SStr _ s1 == SStr _ s2 = s1 == s2

data Program where
  Program :: Pos -> [TopDef] -> Program


data TopDef where
  FnDef :: Pos
        -> Type t
        -> FuncIdent
        -> [Param]
        -> Block
        -> TopDef

  ClassDef  :: Pos
            -> ClassIdent
            -> Maybe ClassIdent
            -> ClassBody
            -> TopDef



instance Eq TopDef where
  (==) (FnDef p1 _ _ _ _)   (FnDef p2 _ _ _ _)  = p1 == p2
  (==) (ClassDef p1 _ _ _)  (ClassDef p2 _ _ _) = p1 == p2
  (==) _ _                                      = False

instance Ord TopDef where
  (<=) def1 def2 = pos def1 <= pos def2 where
    pos (FnDef p _ _ _ _) = p
    pos (ClassDef p _ _ _) = p
  


data Param where 
  Param :: Type a -> Ident a -> Param
  



data Block = Block Pos [Stmt]

data Stmt where
  Empty     :: Pos -> Stmt
  BStmt     :: Pos -> Block -> Stmt
  Decl      :: Pos -> Type a -> [Item a] -> Stmt
  Ass       :: Pos -> Var a -> Expr a -> Stmt
  Incr      :: Pos -> Var a -> Stmt
  Decr      :: Pos -> Var a -> Stmt
  Ret       :: Pos -> Expr a -> Stmt
  VRet      :: Pos -> Stmt
  Cond      :: Pos -> Expr Bool -> Stmt -> Stmt
  CondElse  :: Pos -> Expr Bool -> Stmt -> Stmt -> Stmt
  While     :: Pos -> Expr Bool -> Stmt -> Stmt
  SExp      :: Pos -> Expr a -> Stmt
  For       :: Pos -> Type a -> Ident a -> Var (Array a) -> Stmt -> Stmt



data Item a where
  NoInit  :: Ident a -> Item a
  Init    :: Ident a -> Expr a -> Item a



data Type a where
  Int     :: Pos -> Type Int
  Str     :: Pos -> Type String
  Bool    :: Pos -> Type Bool
  Void    :: Pos -> Type Void
  Arr     :: Type b -> Type (Array b)
  Custom  :: Ident Class -> Type Custom


data Var a where
  Var     :: Pos -> Ident a -> Var a
  Member  :: Pos -> Var a -> Ident t -> Var t
  Elem    :: Pos -> Var (Array a) -> Expr Int -> Var a


data Expr a where
  EVar      :: Pos -> Var b -> Expr b
  ELitInt   :: Pos -> SInt -> Expr Int
  ELitBool  :: Pos -> Bool -> Expr Bool
  EApp      :: Pos -> Var Func -> [Any Expr] -> Expr b
  EString   :: Pos -> SStr -> Expr String
  Neg       :: Pos -> Expr Int -> Expr Int
  Not       :: Pos -> Expr Bool -> Expr Bool
  EOp       :: Pos -> BinOp -> Expr Int -> Expr Int -> Expr Int
  ERel      :: Pos -> RelOp b -> Expr b -> Expr b -> Expr Bool
  EBool     :: Pos -> BoolOp -> Expr Bool -> Expr Bool -> Expr Bool
  NewArr    :: Pos -> Type b -> Expr Int -> Expr (Array b)
  NewObj    :: Pos -> Type Custom -> Expr Custom
  Cast      :: Pos -> Type c -> Expr b -> Expr c



data BinOp = Plus Pos | Minus Pos | Times Pos | Div Pos | Mod Pos
  deriving (Eq, Ord, Show, Read)

data RelOp a where
  LTH :: Pos -> RelOp Int
  LE  :: Pos -> RelOp Int
  GTH :: Pos -> RelOp Int
  GE  :: Pos -> RelOp Int
  EQU :: Pos -> RelOp b
  NE  :: Pos -> RelOp b
  

data BoolOp = And Pos | Or Pos
  deriving (Eq, Ord, Show, Read)




data ClassBody = ClassBody Pos [MemberDecl]


data MemberDecl where
  AttrDecl :: Pos -> Type t -> Ident t -> MemberDecl
  MethodDecl :: TopDef -> MemberDecl
  















