{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module FromBNFC.PrintLatte where

-- pretty-printer generated by the BNF converter

import FromBNFC.AbsLatte
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print PTrue where
  prt _ (PTrue (_,i)) = doc (showString ( i))


instance Print PFalse where
  prt _ (PFalse (_,i)) = doc (showString ( i))


instance Print PTypeInt where
  prt _ (PTypeInt (_,i)) = doc (showString ( i))


instance Print PTypeStr where
  prt _ (PTypeStr (_,i)) = doc (showString ( i))


instance Print PTypeBool where
  prt _ (PTypeBool (_,i)) = doc (showString ( i))


instance Print PTypeVoid where
  prt _ (PTypeVoid (_,i)) = doc (showString ( i))


instance Print PPlus where
  prt _ (PPlus (_,i)) = doc (showString ( i))


instance Print PMinus where
  prt _ (PMinus (_,i)) = doc (showString ( i))


instance Print PTimes where
  prt _ (PTimes (_,i)) = doc (showString ( i))


instance Print PDiv where
  prt _ (PDiv (_,i)) = doc (showString ( i))


instance Print PMod where
  prt _ (PMod (_,i)) = doc (showString ( i))


instance Print PLTH where
  prt _ (PLTH (_,i)) = doc (showString ( i))


instance Print PLE where
  prt _ (PLE (_,i)) = doc (showString ( i))


instance Print PGTH where
  prt _ (PGTH (_,i)) = doc (showString ( i))


instance Print PGE where
  prt _ (PGE (_,i)) = doc (showString ( i))


instance Print PEQU where
  prt _ (PEQU (_,i)) = doc (showString ( i))


instance Print PNE where
  prt _ (PNE (_,i)) = doc (showString ( i))


instance Print PAnd where
  prt _ (PAnd (_,i)) = doc (showString ( i))


instance Print POr where
  prt _ (POr (_,i)) = doc (showString ( i))


instance Print PNot where
  prt _ (PNot (_,i)) = doc (showString ( i))


instance Print PLBrace where
  prt _ (PLBrace (_,i)) = doc (showString ( i))


instance Print PRBrace where
  prt _ (PRBrace (_,i)) = doc (showString ( i))


instance Print PSemiColon where
  prt _ (PSemiColon (_,i)) = doc (showString ( i))


instance Print PIf where
  prt _ (PIf (_,i)) = doc (showString ( i))


instance Print PElse where
  prt _ (PElse (_,i)) = doc (showString ( i))


instance Print PWhile where
  prt _ (PWhile (_,i)) = doc (showString ( i))


instance Print PFor where
  prt _ (PFor (_,i)) = doc (showString ( i))


instance Print PReturn where
  prt _ (PReturn (_,i)) = doc (showString ( i))


instance Print PNew where
  prt _ (PNew (_,i)) = doc (showString ( i))


instance Print PClass where
  prt _ (PClass (_,i)) = doc (showString ( i))


instance Print PExtends where
  prt _ (PExtends (_,i)) = doc (showString ( i))


instance Print PIdent where
  prt _ (PIdent (_,i)) = doc (showString ( i))


instance Print PInteger where
  prt _ (PInteger (_,i)) = doc (showString ( i))


instance Print PString where
  prt _ (PString (_,i)) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Program topdefs -> prPrec i 0 (concatD [prt 0 topdefs])

instance Print TopDef where
  prt i e = case e of
    FnDef type_ pident args block -> prPrec i 0 (concatD [prt 0 type_, prt 0 pident, doc (showString "("), prt 0 args, doc (showString ")"), prt 0 block])
    BaseClassDef pclass pident classbody -> prPrec i 0 (concatD [prt 0 pclass, prt 0 pident, prt 0 classbody])
    ChildClassDef pclass pident1 pextends pident2 classbody -> prPrec i 0 (concatD [prt 0 pclass, prt 0 pident1, prt 0 pextends, prt 0 pident2, prt 0 classbody])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Arg where
  prt i e = case e of
    Arg type_ pident -> prPrec i 0 (concatD [prt 0 type_, prt 0 pident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Block where
  prt i e = case e of
    Block plbrace stmts prbrace -> prPrec i 0 (concatD [prt 0 plbrace, prt 0 stmts, prt 0 prbrace])

instance Print Stmt where
  prt i e = case e of
    Empty psemicolon -> prPrec i 0 (concatD [prt 0 psemicolon])
    BStmt block -> prPrec i 0 (concatD [prt 0 block])
    Decl type_ items psemicolon -> prPrec i 0 (concatD [prt 0 type_, prt 0 items, prt 0 psemicolon])
    Ass var expr psemicolon -> prPrec i 0 (concatD [prt 0 var, doc (showString "="), prt 0 expr, prt 0 psemicolon])
    Incr var psemicolon -> prPrec i 0 (concatD [prt 0 var, doc (showString "++"), prt 0 psemicolon])
    Decr var psemicolon -> prPrec i 0 (concatD [prt 0 var, doc (showString "--"), prt 0 psemicolon])
    Ret preturn expr psemicolon -> prPrec i 0 (concatD [prt 0 preturn, prt 0 expr, prt 0 psemicolon])
    VRet preturn psemicolon -> prPrec i 0 (concatD [prt 0 preturn, prt 0 psemicolon])
    Cond pif expr stmt -> prPrec i 0 (concatD [prt 0 pif, doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    CondElse pif expr stmt1 pelse stmt2 -> prPrec i 0 (concatD [prt 0 pif, doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt1, prt 0 pelse, prt 0 stmt2])
    While pwhile expr stmt -> prPrec i 0 (concatD [prt 0 pwhile, doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 stmt])
    SExp expr psemicolon -> prPrec i 0 (concatD [prt 0 expr, prt 0 psemicolon])
    For pfor type_ pident var stmt -> prPrec i 0 (concatD [prt 0 pfor, doc (showString "("), prt 0 type_, prt 0 pident, doc (showString ":"), prt 0 var, doc (showString ")"), prt 0 stmt])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])
instance Print Item where
  prt i e = case e of
    NoInit pident -> prPrec i 0 (concatD [prt 0 pident])
    Init pident expr -> prPrec i 0 (concatD [prt 0 pident, doc (showString "="), prt 0 expr])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Type where
  prt i e = case e of
    Int ptypeint -> prPrec i 0 (concatD [prt 0 ptypeint])
    Str ptypestr -> prPrec i 0 (concatD [prt 0 ptypestr])
    Bool ptypebool -> prPrec i 0 (concatD [prt 0 ptypebool])
    Void ptypevoid -> prPrec i 0 (concatD [prt 0 ptypevoid])
    Fun type_ types -> prPrec i 0 (concatD [prt 0 type_, doc (showString "("), prt 0 types, doc (showString ")")])
    Arr type_ -> prPrec i 0 (concatD [prt 0 type_, doc (showString "["), doc (showString "]")])
    Custom pident -> prPrec i 0 (concatD [prt 0 pident])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Var where
  prt i e = case e of
    Var pident -> prPrec i 0 (concatD [prt 0 pident])
    Member var pident -> prPrec i 0 (concatD [prt 0 var, doc (showString "."), prt 0 pident])
    Elem var expr -> prPrec i 0 (concatD [prt 0 var, doc (showString "["), prt 0 expr, doc (showString "]")])

instance Print Expr where
  prt i e = case e of
    EVar var -> prPrec i 6 (concatD [prt 0 var])
    ELitInt pinteger -> prPrec i 6 (concatD [prt 0 pinteger])
    ELitTrue ptrue -> prPrec i 6 (concatD [prt 0 ptrue])
    ELitFalse pfalse -> prPrec i 6 (concatD [prt 0 pfalse])
    EApp var exprs -> prPrec i 6 (concatD [prt 0 var, doc (showString "("), prt 0 exprs, doc (showString ")")])
    EString pstring -> prPrec i 6 (concatD [prt 0 pstring])
    Neg pminus expr -> prPrec i 5 (concatD [prt 0 pminus, prt 6 expr])
    Not pnot expr -> prPrec i 5 (concatD [prt 0 pnot, prt 6 expr])
    EMul expr1 mulop expr2 -> prPrec i 4 (concatD [prt 4 expr1, prt 0 mulop, prt 5 expr2])
    EAdd expr1 addop expr2 -> prPrec i 3 (concatD [prt 3 expr1, prt 0 addop, prt 4 expr2])
    ERel expr1 relop expr2 -> prPrec i 2 (concatD [prt 2 expr1, prt 0 relop, prt 3 expr2])
    EAnd expr1 andop expr2 -> prPrec i 1 (concatD [prt 2 expr1, prt 0 andop, prt 1 expr2])
    EOr expr1 orop expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 orop, prt 0 expr2])
    NewArr pnew type_ expr -> prPrec i 0 (concatD [prt 0 pnew, prt 0 type_, doc (showString "["), prt 0 expr, doc (showString "]")])
    NewObj pnew pident -> prPrec i 0 (concatD [prt 0 pnew, prt 0 pident])
    Cast type_ expr -> prPrec i 5 (concatD [doc (showString "("), prt 0 type_, doc (showString ")"), prt 6 expr])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print AddOp where
  prt i e = case e of
    Plus pplus -> prPrec i 0 (concatD [prt 0 pplus])
    Minus pminus -> prPrec i 0 (concatD [prt 0 pminus])

instance Print MulOp where
  prt i e = case e of
    Times ptimes -> prPrec i 0 (concatD [prt 0 ptimes])
    Div pdiv -> prPrec i 0 (concatD [prt 0 pdiv])
    Mod pmod -> prPrec i 0 (concatD [prt 0 pmod])

instance Print RelOp where
  prt i e = case e of
    LTH plth -> prPrec i 0 (concatD [prt 0 plth])
    LE ple -> prPrec i 0 (concatD [prt 0 ple])
    GTH pgth -> prPrec i 0 (concatD [prt 0 pgth])
    GE pge -> prPrec i 0 (concatD [prt 0 pge])
    EQU pequ -> prPrec i 0 (concatD [prt 0 pequ])
    NE pne -> prPrec i 0 (concatD [prt 0 pne])

instance Print AndOp where
  prt i e = case e of
    And pand -> prPrec i 0 (concatD [prt 0 pand])

instance Print OrOp where
  prt i e = case e of
    Or por -> prPrec i 0 (concatD [prt 0 por])

instance Print ClassBody where
  prt i e = case e of
    ClassBody plbrace memberdecls prbrace -> prPrec i 0 (concatD [prt 0 plbrace, prt 0 memberdecls, prt 0 prbrace])

instance Print MemberDecl where
  prt i e = case e of
    AttrDecl type_ pident psemicolon -> prPrec i 0 (concatD [prt 0 type_, prt 0 pident, prt 0 psemicolon])
    MethodDecl topdef -> prPrec i 0 (concatD [prt 0 topdef])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, prt 0 xs])

