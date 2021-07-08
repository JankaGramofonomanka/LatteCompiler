module FromBNFC.SkelLatte where

-- Haskell module generated by the BNF converter

import FromBNFC.AbsLatte
import FromBNFC.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transPTrue :: PTrue -> Result
transPTrue x = case x of
  PTrue string -> failure x
transPFalse :: PFalse -> Result
transPFalse x = case x of
  PFalse string -> failure x
transPReturn :: PReturn -> Result
transPReturn x = case x of
  PReturn string -> failure x
transPTypeInt :: PTypeInt -> Result
transPTypeInt x = case x of
  PTypeInt string -> failure x
transPTypeStr :: PTypeStr -> Result
transPTypeStr x = case x of
  PTypeStr string -> failure x
transPTypeBool :: PTypeBool -> Result
transPTypeBool x = case x of
  PTypeBool string -> failure x
transPTypeVoid :: PTypeVoid -> Result
transPTypeVoid x = case x of
  PTypeVoid string -> failure x
transPPlus :: PPlus -> Result
transPPlus x = case x of
  PPlus string -> failure x
transPMinus :: PMinus -> Result
transPMinus x = case x of
  PMinus string -> failure x
transPTimes :: PTimes -> Result
transPTimes x = case x of
  PTimes string -> failure x
transPDiv :: PDiv -> Result
transPDiv x = case x of
  PDiv string -> failure x
transPMod :: PMod -> Result
transPMod x = case x of
  PMod string -> failure x
transPLTH :: PLTH -> Result
transPLTH x = case x of
  PLTH string -> failure x
transPLE :: PLE -> Result
transPLE x = case x of
  PLE string -> failure x
transPGTH :: PGTH -> Result
transPGTH x = case x of
  PGTH string -> failure x
transPGE :: PGE -> Result
transPGE x = case x of
  PGE string -> failure x
transPEQU :: PEQU -> Result
transPEQU x = case x of
  PEQU string -> failure x
transPNE :: PNE -> Result
transPNE x = case x of
  PNE string -> failure x
transPIdent :: PIdent -> Result
transPIdent x = case x of
  PIdent string -> failure x
transPInteger :: PInteger -> Result
transPInteger x = case x of
  PInteger string -> failure x
transPString :: PString -> Result
transPString x = case x of
  PString string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x
transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type_ pident args block -> failure x
transArg :: Arg -> Result
transArg x = case x of
  Arg type_ pident -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass pident expr -> failure x
  Incr pident -> failure x
  Decr pident -> failure x
  Ret preturn expr -> failure x
  VRet preturn -> failure x
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  SExp expr -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit pident -> failure x
  Init pident expr -> failure x
transType :: Type -> Result
transType x = case x of
  Int ptypeint -> failure x
  Str ptypestr -> failure x
  Bool ptypebool -> failure x
  Void ptypevoid -> failure x
  Fun type_ types -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar pident -> failure x
  ELitInt pinteger -> failure x
  ELitTrue ptrue -> failure x
  ELitFalse pfalse -> failure x
  EApp pident exprs -> failure x
  EString pstring -> failure x
  Neg pminus expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus pplus -> failure x
  Minus pminus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times ptimes -> failure x
  Div pdiv -> failure x
  Mod pmod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH plth -> failure x
  LE ple -> failure x
  GTH pgth -> failure x
  GE pge -> failure x
  EQU pequ -> failure x
  NE pne -> failure x

