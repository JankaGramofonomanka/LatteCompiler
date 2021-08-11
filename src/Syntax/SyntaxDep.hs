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

deriving instance Show (SLatteType t)

type Any a = Sigma LatteType (TyCon1 a)


type Ident :: LatteType -> Type
data Ident a = Ident Pos String deriving (Ord, Show)

type FuncIdent :: LatteType -> [LatteType] -> Type
data FuncIdent t ts = FuncIdent Pos String deriving (Ord, Show)

type ClassIdent :: Str -> Type
data ClassIdent cls = ClassIdent Pos (SStr cls) deriving (Ord, Show)


instance Eq (Ident a) where
  Ident _ x == Ident _ y = x == y

instance Eq (FuncIdent t ts) where
  FuncIdent _ x == FuncIdent _ y = x == y

instance Eq (ClassIdent t) where
  ClassIdent _ x == ClassIdent _ y = x == y



data Program where
  Program :: Pos -> [Either ClassDef FnDef] -> Program

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

data Param t = Param (TypeKW t) (Ident t)
type ParamList ts = DList Param ts
type SomeClassIdent = Sigma Str (TyCon1 ClassIdent)
  


data Block = Block Pos [Stmt]

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
  For       :: Pos -> TypeKW t -> Ident t -> Var (Arr t) -> Stmt -> Stmt



data Item t where
  NoInit  :: Ident t -> Item t
  Init    :: Ident t -> Expr t -> Item t


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


type Var :: LatteType -> Type
data Var a where
  Var     :: Pos -> Ident a -> Var a
  Attr    :: Pos -> Expr a -> Ident t -> Var t
  Elem    :: Pos -> Expr (Arr a) -> Expr TInt -> Var a
  Null    :: Pos -> Var TNull
  Self    :: Pos -> Var (Custom cls)


type Callable :: LatteType -> [LatteType] -> Type
data Callable t ts where
  Func    :: Pos -> FuncIdent t ts -> Callable t ts
  Method  :: Pos -> Expr (Custom n) -> FuncIdent t ts -> Callable t ts




type Expr :: LatteType -> Type
data Expr a where
  EVar      :: Pos -> Sing t -> Var t -> Expr t
  ELitInt   :: Pos -> Int -> Expr TInt
  ELitBool  :: Pos -> Bool -> Expr TBool
  EApp      :: Pos -> Sing t -> Callable t ts -> ExprList ts -> Expr t
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



data ClassBody = ClassBody Pos [MemberDecl]


data MemberDecl where
  AttrDecl :: Pos -> TypeKW t -> Ident t -> MemberDecl
  MethodDecl :: FnDef -> MemberDecl
  















