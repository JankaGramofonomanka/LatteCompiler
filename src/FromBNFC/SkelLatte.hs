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
transPAnd :: PAnd -> Result
transPAnd x = case x of
  PAnd string -> failure x
transPOr :: POr -> Result
transPOr x = case x of
  POr string -> failure x
transPNot :: PNot -> Result
transPNot x = case x of
  PNot string -> failure x
transPLBrace :: PLBrace -> Result
transPLBrace x = case x of
  PLBrace string -> failure x
transPRBrace :: PRBrace -> Result
transPRBrace x = case x of
  PRBrace string -> failure x
transPSemiColon :: PSemiColon -> Result
transPSemiColon x = case x of
  PSemiColon string -> failure x
transPIf :: PIf -> Result
transPIf x = case x of
  PIf string -> failure x
transPElse :: PElse -> Result
transPElse x = case x of
  PElse string -> failure x
transPWhile :: PWhile -> Result
transPWhile x = case x of
  PWhile string -> failure x
transPFor :: PFor -> Result
transPFor x = case x of
  PFor string -> failure x
transPReturn :: PReturn -> Result
transPReturn x = case x of
  PReturn string -> failure x
transPNew :: PNew -> Result
transPNew x = case x of
  PNew string -> failure x
transPClass :: PClass -> Result
transPClass x = case x of
  PClass string -> failure x
transPExtends :: PExtends -> Result
transPExtends x = case x of
  PExtends string -> failure x
transPNull :: PNull -> Result
transPNull x = case x of
  PNull string -> failure x
transPSelf :: PSelf -> Result
transPSelf x = case x of
  PSelf string -> failure x
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
  FnDef type_ pident params block -> failure x
  BaseClassDef pclass pident classbody -> failure x
  ChildClassDef pclass pident1 pextends pident2 classbody -> failure x
transParam :: Param -> Result
transParam x = case x of
  Param type_ pident -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block plbrace stmts prbrace -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty psemicolon -> failure x
  BStmt block -> failure x
  Decl type_ items psemicolon -> failure x
  Ass var expr psemicolon -> failure x
  Incr var psemicolon -> failure x
  Decr var psemicolon -> failure x
  Ret preturn expr psemicolon -> failure x
  VRet preturn psemicolon -> failure x
  Cond pif expr stmt -> failure x
  CondElse pif expr stmt1 pelse stmt2 -> failure x
  While pwhile expr stmt -> failure x
  SExp expr psemicolon -> failure x
  For pfor type_ pident expr stmt -> failure x
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
  Arr type_ -> failure x
  Custom pident -> failure x
transVar :: Var -> Result
transVar x = case x of
  Var pident -> failure x
  Member expr pident -> failure x
  Elem expr1 expr2 -> failure x
  Null pnull -> failure x
  Self pself -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar var -> failure x
  ELitInt pinteger -> failure x
  ELitTrue ptrue -> failure x
  ELitFalse pfalse -> failure x
  EApp var exprs -> failure x
  EString pstring -> failure x
  Neg pminus expr -> failure x
  Not pnot expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 andop expr2 -> failure x
  EOr expr1 orop expr2 -> failure x
  NewArr pnew type_ expr -> failure x
  NewObj pnew pident -> failure x
  Cast type_ expr -> failure x
  CastE expr1 expr2 -> failure x
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
transAndOp :: AndOp -> Result
transAndOp x = case x of
  And pand -> failure x
transOrOp :: OrOp -> Result
transOrOp x = case x of
  Or por -> failure x
transClassBody :: ClassBody -> Result
transClassBody x = case x of
  ClassBody plbrace memberdecls prbrace -> failure x
transMemberDecl :: MemberDecl -> Result
transMemberDecl x = case x of
  AttrDecl type_ pident psemicolon -> failure x
  MethodDecl topdef -> failure x

