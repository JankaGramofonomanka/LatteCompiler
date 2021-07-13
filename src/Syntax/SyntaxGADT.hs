{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Syntax.SyntaxGADT where



type Pos = (Int, Int)
newtype Void = Vd ()
newtype Custom = Cst ()
newtype Func = Func ()
data MemberId t where
  AttrId :: Var a -> Ident t -> MemberId t
  MethId :: Var a -> FuncIdent -> MemberId Func

newtype Class = Class ()
newtype Array a = Array ()

data Any (a :: * -> *) where
  --Any :: a b -> Any a
  Any :: Type b -> a b -> Any a


  
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

  --deriving (Show, Read)

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
  
  -- deriving (Eq, Ord, Show, Read)


data Block = Block Pos [Stmt] -- deriving (Eq, Ord, Show, Read)

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

  -- deriving (Eq, Ord, Show, Read)

data Item a where
  NoInit  :: Ident a -> Item a
  Init    :: Ident a -> Expr a -> Item a

  -- deriving (Eq, Ord, Show, Read)

data Type a where
  Int     :: Pos -> Type Int
  Str     :: Pos -> Type String
  Bool    :: Pos -> Type Bool
  Void    :: Pos -> Type Void
  Arr     :: Type b -> Type (Array b)
  Custom  :: Ident Custom -> Type Custom

  -- deriving (Eq, Ord, Show, Read)

data Var a where
  Var     :: Pos -> Ident a -> Var a
  Fun     :: Pos -> FuncIdent -> Var Func
  Member  :: Pos -> MemberId t -> Var t
  Elem    :: Pos -> Var (Array a) -> Expr Int -> Var a
  
  -- deriving (Eq, Ord, Show, Read)

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
  NewObj    :: Pos -> Ident b -> Expr b
  Cast      :: Pos -> Type c -> Expr b -> Expr c

  -- deriving (Eq, Ord, Show, Read)

--data AnyExpr where
--  AnyExpr :: Pos -> Expr a -> AnyExpr

data BinOp = BinOp Pos  (Int -> Int -> Int)

data RelOp a = RelOp Pos (a -> a -> Bool)

type BoolOp = RelOp Bool



data ClassBody = ClassBody Pos [MemberDecl]
  -- deriving (Eq, Ord, Show, Read)

data MemberDecl where
  AttrDecl :: Pos -> Type t -> Ident t -> MemberDecl
  MethodDecl :: TopDef -> MemberDecl
  
  -- deriving (Eq, Ord, Show, Read)














