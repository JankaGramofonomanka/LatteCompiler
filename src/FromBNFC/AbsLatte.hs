

module FromBNFC.AbsLatte where

-- Haskell module generated by the BNF converter




newtype PTrue = PTrue ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PFalse = PFalse ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PReturn = PReturn ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PTypeInt = PTypeInt ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PTypeStr = PTypeStr ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PTypeBool = PTypeBool ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PTypeVoid = PTypeVoid ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PPlus = PPlus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PMinus = PMinus ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PTimes = PTimes ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PDiv = PDiv ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PMod = PMod ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PLTH = PLTH ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PLE = PLE ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype PGTH = PGTH ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PGE = PGE ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype PEQU = PEQU ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PNE = PNE ((Int,Int),String) deriving (Eq, Ord, Show, Read)
newtype PIdent = PIdent ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PInteger = PInteger ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
newtype PString = PString ((Int,Int),String)
  deriving (Eq, Ord, Show, Read)
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type PIdent [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type PIdent
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass PIdent Expr
    | Incr PIdent
    | Decr PIdent
    | Ret PReturn Expr
    | VRet PReturn
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit PIdent | Init PIdent Expr
  deriving (Eq, Ord, Show, Read)

data Type
    = Int PTypeInt
    | Str PTypeStr
    | Bool PTypeBool
    | Void PTypeVoid
    | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar PIdent
    | ELitInt PInteger
    | ELitTrue PTrue
    | ELitFalse PFalse
    | EApp PIdent [Expr]
    | EString PString
    | Neg PMinus Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus PPlus | Minus PMinus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times PTimes | Div PDiv | Mod PMod
  deriving (Eq, Ord, Show, Read)

data RelOp
    = LTH PLTH | LE PLE | GTH PGTH | GE PGE | EQU PEQU | NE PNE
  deriving (Eq, Ord, Show, Read)

