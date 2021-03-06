{-# LANGUAGE 
    GADTs
  , KindSignatures
  , TemplateHaskell
  , StandaloneKindSignatures
  , DataKinds
  , ScopedTypeVariables
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , StandaloneDeriving

  , FlexibleInstances
#-}

module Syntax.SyntaxDep where

import Data.Singletons.TH
import Data.Singletons.Sigma
import Data.Singletons.Prelude
import Data.Kind ( Type )
import Data.Singletons.Prelude.List (LastSym0, Last)

import Position.Position ( Pos )
import Dependent
import SingChar




data LatteType
  = TInt
  | TStr
  | TBool
  | TVoid
  | Arr LatteType
  | Custom Str
  | TNull

  deriving Show

genSingletons [''LatteType]

type Any a = Sigma LatteType (TyCon1 a)


type Ident :: LatteType -> Type
data Ident t = Ident Pos String deriving (Ord, Show)

type ScopedIdent :: LatteType -> Type
data ScopedIdent t
  = Scoped Int (Ident t)
  | SelfAttr (Ident t)

  deriving (Ord, Show)

type FuncIdent :: LatteType -> [LatteType] -> Type
data FuncIdent t ts = FuncIdent Pos String deriving (Ord, Show)

type ClassIdent :: Str -> Type
data ClassIdent cls = ClassIdent Pos (SStr cls) deriving (Ord, Show)


instance Eq (Ident t) where
  Ident _ x == Ident _ y = x == y

instance Eq (ScopedIdent t) where
  Scoped i x == Scoped j y = i == j && x == y
  SelfAttr x == SelfAttr y = x == y
  _ == _ = False

instance Eq (FuncIdent t ts) where
  FuncIdent _ x == FuncIdent _ y = x == y

instance Eq (ClassIdent t) where
  ClassIdent _ x == ClassIdent _ y = x == y



data Program = Program Pos [Either ClassDef FnDef] deriving Show

data FnDef where
  FnDef :: Pos
        -> TypeKW t
        -> FuncIdent t ts
        -> ParamList ts
        -> Block
        -> FnDef

data ClassDef where
  ClassDef  :: Pos
            -> ClassIdent cls
            -> Maybe SomeClassIdent
            -> ClassBody
            -> ClassDef

data Param t = Param (TypeKW t) (Ident t) deriving Show
type ParamList ts = DList Param ts
type SomeClassIdent = Sigma Str (TyCon1 ClassIdent)
  


data Block = Block Pos Pos [Stmt] deriving Show

data Stmt where
  Empty     :: Pos -> Stmt
  BStmt     :: Pos -> Block -> Stmt
  Decl      :: Pos -> TypeKW t -> [Item t] -> Stmt
  Ass       :: Pos -> Sing t -> Var t -> Expr t -> Stmt
  Incr      :: Pos -> Var TInt -> Stmt
  Decr      :: Pos -> Var TInt -> Stmt
  Ret       :: Pos -> Sing t -> Expr t -> Stmt
  VRet      :: Pos -> Stmt
  Cond      :: Pos -> Expr TBool -> Stmt -> Stmt
  CondElse  :: Pos -> Expr TBool -> Stmt -> Stmt -> Stmt
  While     :: Pos -> Expr TBool -> Stmt -> Stmt
  SExp      :: Pos -> Sing t -> Expr t -> Stmt
  For       :: Pos -> TypeKW t -> ScopedIdent t -> Expr (Arr t) -> Stmt -> Stmt

  -- a statement to convert `while true` loops to
  Forever   :: Pos -> Stmt -> Stmt



data Item t where
  NoInit  :: ScopedIdent t -> Item t
  Init    :: ScopedIdent t -> Expr t -> Item t


type TypeKW :: LatteType -> Type
data TypeKW t where
  KWInt   :: Pos -> TypeKW TInt
  KWStr   :: Pos -> TypeKW TStr
  KWBool  :: Pos -> TypeKW TBool
  KWVoid  :: Pos -> TypeKW TVoid
  
  KWArr :: TypeKW t -> TypeKW (Arr t)

  KWCustom :: ClassIdent cls -> TypeKW (Custom cls)


singFromKW :: TypeKW t -> Sing t
singFromKW kw = case kw of
  KWInt   _ -> STInt
  KWStr   _ -> STStr
  KWBool  _ -> STBool
  KWVoid  _ -> STVoid
  
  KWArr kw -> SArr (singFromKW kw)

  KWCustom (ClassIdent _ cls) -> SCustom cls

kwFromSing :: Pos -> Sing t -> TypeKW t
kwFromSing p t = case t of
  STInt   -> KWInt p
  STStr   -> KWStr p
  STBool  -> KWBool p
  STVoid  -> KWVoid p
  
  SArr tt -> KWArr (kwFromSing p tt)

  SCustom cls -> KWCustom (ClassIdent p cls)

  STNull -> error "INTERNAL ERROR: keyword made from null"


type Var :: LatteType -> Type
data Var a where
  Var     :: Pos -> ScopedIdent t -> Var t
  Attr    :: Pos -> Sing (Custom cls) -> Expr (Custom cls) -> Ident t -> Var t
  Length  :: Pos -> Sing (Arr t) -> Expr (Arr t) -> Var TInt
  Elem    :: Pos -> Expr (Arr t) -> Expr TInt -> Var t
  Null    :: Pos -> Var TNull
  Self    :: Pos -> Var (Custom cls)


type Callable :: LatteType -> [LatteType] -> Type
data Callable t ts where
  Func    :: Pos -> FuncIdent t ts -> Callable t ts
  Method  :: Pos
          -> Sing (Custom cls)
          -> Expr (Custom cls)
          -> FuncIdent t ts
          -> Callable t ts




type Expr :: LatteType -> Type
data Expr a where
  EVar      :: Pos -> Sing t -> Var t -> Expr t
  ELitInt   :: Pos -> Int -> Expr TInt
  ELitBool  :: Pos -> Bool -> Expr TBool
  EApp      :: Pos
            -> Sing t
            -> Callable t ts
            -> SList ts
            -> ExprList ts
            -> Expr t
            
  EString   :: Pos -> String -> Expr TStr
  Neg       :: Pos -> Expr TInt -> Expr TInt
  Not       :: Pos -> Expr TBool -> Expr TBool
  EOp       :: Pos -> BinOp -> Expr TInt -> Expr TInt -> Expr TInt
  ERel      :: Pos -> Sing t -> RelOp t -> Expr t -> Expr t -> Expr TBool
  EBool     :: Pos -> BoolOp -> Expr TBool -> Expr TBool -> Expr TBool
  NewArr    :: Pos -> TypeKW t -> Expr TInt -> Expr (Arr t)
  NewObj    :: Pos -> TypeKW (Custom cls) -> Expr (Custom cls)
  Cast      :: Pos -> TypeKW t1 -> Expr t2 -> Expr t1
  Concat    :: Pos -> Expr TStr -> Expr TStr -> Expr TStr

type ExprList ts = DList Expr ts

exprType :: Expr t -> Sing t
exprType expr = case expr of
  EVar      _ t _       -> t
  ELitInt   {}          -> STInt
  ELitBool  {}          -> STBool
  EApp      _ t _ _ _   -> t
            
  EString   {}          -> STStr
  Neg       {}          -> STInt
  Not       {}          -> STBool
  EOp       {}          -> STInt
  ERel      {}          -> STBool
  EBool     {}          -> STBool
  NewArr    _ typeKW _  -> SArr (singFromKW typeKW)
  NewObj    _ typeKW    -> singFromKW typeKW
  Cast      _ typeKW _  -> singFromKW typeKW
  Concat    {}          -> STStr


data BinOp = Plus Pos | Minus Pos | Times Pos | Div Pos | Mod Pos
  deriving (Eq, Ord, Show)

type RelOp :: LatteType -> Type
data RelOp a where
  LTH :: Pos -> RelOp TInt
  LE  :: Pos -> RelOp TInt
  GTH :: Pos -> RelOp TInt
  GE  :: Pos -> RelOp TInt
  EQU :: Pos -> RelOp t
  NE  :: Pos -> RelOp t
  

data BoolOp = And Pos | Or Pos
  deriving (Eq, Ord, Show)



data ClassBody = ClassBody Pos Pos [MemberDecl] deriving Show


data MemberDecl where
  AttrDecl :: Pos -> TypeKW t -> Ident t -> MemberDecl
  MethodDecl :: FnDef -> MemberDecl
  






deriving instance Show (SLatteType t)
deriving instance Show FnDef
deriving instance Show ClassDef
deriving instance Show Stmt
deriving instance Show (Item t)
deriving instance Show (TypeKW t)
deriving instance Show (Var t)
deriving instance Show (Callable t ts)
deriving instance Show (Expr t)
deriving instance Show (RelOp t)
deriving instance Show MemberDecl

deriving instance Show (DList Param ts)
deriving instance Show (DList Expr ts)



