{-# LANGUAGE GADTs #-}

module Syntax.Syntax where



type Pos = (Int, Int)

  
data Ident = Ident Pos String deriving (Eq, Ord, Show, Read)

data SInt = SInt Pos Int deriving (Eq, Ord, Show, Read)

data SStr = SStr Pos String deriving (Eq, Ord, Show, Read)

data Program where
  Program :: Pos -> [TopDef] -> Program


data TopDef where
  FnDef :: Pos
        -> Type
        -> Ident
        -> [Param]
        -> Block
        -> TopDef

  ClassDef  :: Pos
            -> Ident
            -> Maybe Ident
            -> ClassBody
            -> TopDef

  deriving (Eq, Ord, Show, Read) 


data Param = Param Type Ident deriving (Eq, Ord, Show, Read)

data Block = Block Pos [Stmt] deriving (Eq, Ord, Show, Read)

data Stmt where
  Empty     :: Pos -> Stmt
  BStmt     :: Pos -> Block -> Stmt
  Decl      :: Pos -> Type -> [Item] -> Stmt
  Ass       :: Pos -> Var -> Expr -> Stmt
  Incr      :: Pos -> Var -> Stmt
  Decr      :: Pos -> Var -> Stmt
  Ret       :: Pos -> Expr -> Stmt
  VRet      :: Pos -> Stmt
  Cond      :: Pos -> Expr -> Stmt -> Stmt
  CondElse  :: Pos -> Expr -> Stmt -> Stmt -> Stmt
  While     :: Pos -> Expr -> Stmt -> Stmt
  SExp      :: Pos -> Expr -> Stmt
  For       :: Pos -> Type -> Ident -> Expr -> Stmt -> Stmt

  deriving (Eq, Ord, Show, Read)

data Item where
  NoInit  :: Ident -> Item
  Init    :: Ident -> Expr -> Item

  deriving (Eq, Ord, Show, Read)

data Type where
  Int     :: Pos -> Type
  Str     :: Pos -> Type
  Bool    :: Pos -> Type
  Void    :: Pos -> Type
  Arr     :: Type -> Type
  Custom  :: Ident -> Type

  deriving (Eq, Ord, Show, Read)

data Var where
  Var     :: Pos -> Ident -> Var
  Member  :: Pos -> Expr -> Ident -> Var
  Elem    :: Pos -> Expr -> Expr -> Var
  Null    :: Pos -> Var
  Self    :: Pos -> Var
  
  deriving (Eq, Ord, Show, Read)

data Expr where
  EVar      :: Pos -> Var -> Expr
  ELitInt   :: Pos -> SInt -> Expr
  ELitBool  :: Pos -> Bool -> Expr
  EApp      :: Pos -> Var -> [Expr] -> Expr
  EString   :: Pos -> SStr -> Expr
  Neg       :: Pos -> Expr -> Expr
  Not       :: Pos -> Expr -> Expr
  EOp       :: Pos -> BinOp -> Expr -> Expr -> Expr
  ERel      :: Pos -> RelOp -> Expr -> Expr -> Expr
  EBool     :: Pos -> BoolOp -> Expr -> Expr -> Expr
  NewArr    :: Pos -> Type -> Expr -> Expr
  NewObj    :: Pos -> Ident -> Expr
  Cast      :: Pos -> Type -> Expr -> Expr

  deriving (Eq, Ord, Show, Read)


data BinOp = Plus Pos | Minus Pos | Times Pos | Div Pos | Mod Pos
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH Pos | LE Pos | GTH Pos | GE Pos | EQU Pos | NE Pos
  deriving (Eq, Ord, Show, Read)

data BoolOp = And Pos | Or Pos
  deriving (Eq, Ord, Show, Read)



data ClassBody = ClassBody Pos [MemberDecl]
  deriving (Eq, Ord, Show, Read)

data MemberDecl where
  AttrDecl :: Pos -> Type -> Ident -> MemberDecl
  MethodDecl :: TopDef -> MemberDecl
  
  deriving (Eq, Ord, Show, Read)










