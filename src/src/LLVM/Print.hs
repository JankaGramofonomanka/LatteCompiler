{-# LANGUAGE
    GADTs
  , ScopedTypeVariables
  , DataKinds
  , KindSignatures
  , PolyKinds
  , RankNTypes
  , TypeApplications
  , RecordWildCards
#-}

module LLVM.Print where

import qualified Data.Map as M
import Prelude hiding ( EQ )
import Data.Singletons.Sigma
import Data.Singletons.TypeLits
import Data.Singletons.Prelude hiding ( SGT, SLT )

import LLVM.LLVM
import qualified Constants as C
import Dependent
import SingChar
import GHC.Base (eqString)
import LLVM.LLVM (SPrimType(SVTable))


tab :: Int -> String -> String
tab n s = replicate n ' ' ++ s

paste :: String -> [String] -> String
paste _ [] = ""
paste _ [x] = x
paste sep (x : xs) = x ++ sep ++ paste sep xs

class SimplePrint a where
  prt :: a -> String

sPaste :: String -> SList (xs :: [PrimType]) -> String
sPaste sep SNil = ""
sPaste sep (SCons x SNil) = prt x
sPaste sep (SCons x xs) = prt x ++ sep ++ sPaste sep xs

prtSomeType :: Some SPrimType -> String
prtSomeType (Some x) = prt x


-- Primitive Types ------------------------------------------------------------


printSNat :: SNat n -> String
printSNat n = drop 6 (show n)

fromSNatural :: SNatural z -> Integer
fromSNatural SZero = 0
fromSNatural (SSucc n) = fromSNatural n + 1

printSNatural :: SNatural z -> String
printSNatural n = show (fromSNatural n)


instance SimplePrint (SPrimType t) where
  prt (SI n)          = "i" ++ printSNat n
  prt SVoid           = "void"
  prt (SPtr t)        = prt t ++ "*"
  prt (SArray t n)    = "[" ++ printSNatural n ++ " x " ++ prt t ++ "]"
  prt (SStruct s)     = "%" ++ singToString s
  prt (SArrStruct t)  = "%" ++ mkArrStructName t
  prt (SVTable s)     = "%" ++ C.mkVtableType (singToString s)
  prt (SFuncType retT paramTs)
    = paste "" [prt retT, "(", sPaste ", " paramTs, ")"]

mkArrStructName :: SPrimType t -> String
mkArrStructName t
  = C.arrStructPrefix ++ "." ++ prt' t ++ "." ++ show (dim t) where
  
    prt' :: SPrimType t -> String
    prt' t = case t of
      SArray tt _ -> prt' tt
      SStruct s   -> singToString s
      SPtr tt     -> prt' tt ++ "ptr"
      _           -> prt t
    
    dim :: SPrimType t -> Int
    dim t = case t of
      SArray tt _ -> 1 + dim tt
      _           -> 0


-- Simple Values --------------------------------------------------------------
instance SimplePrint (Reg t) where
  prt (Reg s i) = "%" ++ s ++ "." ++  show i
  prt (SpecialReg s) = "%" ++ s

instance SimplePrint (Constant t) where
  prt (Const s i) = "@" ++ s ++ "." ++ show i
  prt (SpecialConst s) = "@" ++ s

instance SimplePrint (Value t) where
  prt (Var reg) = prt reg
  prt (ILit i) = show i
  prt (BoolLit True) = "true"
  prt (BoolLit False) = "false"
  prt (ConstPtr cst) = prt cst
  prt Null = "null"
  prt (FuncConst l) = prt l


-- Labels ---------------------------------------------------------------------
instance SimplePrint Label where
  prt (Label s i) = s ++ "." ++ show i

prtVarLabel :: Label -> String
prtVarLabel l = "%" ++ prt l

instance SimplePrint (FuncLabel t ts) where
  prt (FuncLabel s) = "@" ++ s


-- Operators ------------------------------------------------------------------

instance SimplePrint (BinOp t) where
  prt t = case t of
    ADD   -> "add"
    SUB   -> "sub"
    MUL   -> "mul"
    SDIV  -> "sdiv"
    UDIV  -> "udiv"
    SREM  -> "srem"
    UREM  -> "urem"

instance SimplePrint (BitOp t) where
  prt t = case t of
    SHL  -> "shl"
    LSHR -> "lshr"
    ASHR -> "ashr"
    AND  -> "and"
    OR   -> "or"
    XOR  -> "xor"

instance SimplePrint CMPKind where
  prt op = case op of
    EQ  -> "eq"
    NE  -> "ne"
    SGT -> "sgt"
    SGE -> "sge"
    SLT -> "slt"
    SLE -> "sle"
    UGT -> "ugt"
    UGE -> "uge"
    ULT -> "ult"
    ULE -> "ule"



-- Expression -----------------------------------------------------------------

instance SimplePrint (Expr t) where

  prt (BinOperation t op lhs rhs)
    = paste " " [prt op, prt t, prt lhs ++ ",", prt rhs]

  prt (Call t f argTs args)
    = paste " " ["call", prt t, prt f ++ "(" ++ prtArgs argTs args ++ ")"]

    where
      prtArgs :: SList ts -> ArgList ts -> String
      prtArgs SNil DNil = ""
      prtArgs (SCons t SNil) (arg :> DNil) = paste " " [prt t, prt arg]
      prtArgs (SCons t ts) (arg :> args)
        = paste " " [prt t, prt arg] ++ ", " ++ prtArgs ts args
      
  prt (GetElemPtr contT indexT container index)
    = prtGetElemPtr contT container indexTypes indices where
      indexTypes = indexT :> DNil
      indices = index :> DNil
    
  prt (GetArrElemPtr contT indexT1 indexT2 container index1 index2)
    = prtGetElemPtr contT container indexTypes indices where
      indexTypes = indexT1 :> indexT2 :> DNil
      indices = index1 :> index2 :> DNil

  prt (GetAttrPtr ownerT indexT1 indexT2 owner index1 index2)
    = prtGetElemPtr ownerT owner indexTypes indices where
      indexTypes = indexT1 :> indexT2 :> DNil
      indices = index1 :> index2 :> DNil

  prt (GetArrAttrPtr arrT indexT1 indexT2 arr index1 index2)
    = prtGetElemPtr arrT arr indexTypes indices where
      indexTypes = indexT1 :> indexT2 :> DNil
      indices = index1 :> index2 :> DNil
    
  prt (ICMP t kind lhs rhs)
    = paste " " ["icmp", prt kind, prt t, prt lhs ++ ",", prt rhs]

  prt (Phi t vals)
    = paste " " ["phi", prt t, paste ", " (map prtValTuple vals)]

    where
      prtValTuple (label, val)
        = "[" ++ prt val ++ ", " ++ prtVarLabel label ++ "]"

  prt (Load singT ptr)
    = paste " " ["load", prt singT ++ ",", prt (SPtr singT), prt ptr]

  prt (BitCast t1 val t2)
    = paste " " ["bitcast", prt t1 , prt val, "to", prt t2]

prtGetElemPtr
  :: Sing t -> Value (Ptr t) -> DList SPrimType ts -> DList Value ts -> String
prtGetElemPtr contT container indexTypes indices = paste " "
  $ [ "getelementptr"
    , prt contT ++ ","
    , prt (SPtr contT)
    , prt container ++ ","
    ]
  ++ indiceKws indexTypes indices
    
    where
      indiceKws :: DList SPrimType ts -> DList Value ts -> [String]
      indiceKws DNil DNil = []
      indiceKws (t :> DNil) (i :> DNil) = [prt t, prt i]
      indiceKws (t :> ts) (i :> is) = [prt t, prt i ++ ","] ++ indiceKws ts is


-- Instructions ---------------------------------------------------------------

instance SimplePrint SimpleInstr where
  prt (Ass reg expr) = paste " " [prt reg, "=", prt expr]
  prt (VoidExpr expr) = prt expr
  prt (Store singT val ptr) = paste " " 
    ["store", prt singT, prt val ++ ",", prt (SPtr singT), prt ptr]

prtSimpleInstr :: Int -> SimpleInstr -> String
prtSimpleInstr n instr = tab n (prt instr)

instance SimplePrint BranchInstr where
  prt (Branch l) = paste " " ["br label", prtVarLabel l]
  prt (CondBranch cond labelIf labelElse) = paste " " 
    [ "br"
    , prt i1
    , prt cond ++ ", label"
    , prtVarLabel labelIf ++ ", label"
    , prtVarLabel labelElse
    ]

  prt (Ret t v) = paste " " ["ret", prt t, prt v]
  prt RetVoid = "ret void"

prtBranchInstr :: Int -> BranchInstr -> String
prtBranchInstr n instr = tab n (prt instr)


prtComSimpleInstr :: Int -> Int -> ComSimpleInstr -> String
prtComSimpleInstr n m (Comment cmt) = "\n" ++ tab n ("; " ++ cmt)
prtComSimpleInstr n m (Instr instr Nothing) = tab n (prt instr)
prtComSimpleInstr n m (Instr instr (Just cmt))
  = tab n (prt instr ++ replicate mm ' ' ++ "; " ++ cmt) where
      mm = (m - n) - length (prt instr)

prtComBranchInstr :: Int -> Int -> ComBranchInstr -> String
prtComBranchInstr n m (BrInstr instr Nothing) = tab n (prt instr)
prtComBranchInstr n m (BrInstr instr (Just cmt))
  = tab n (prt instr ++ replicate mm ' ' ++ "; " ++ cmt) where
      mm = (m - n) - length (prt instr)


-- Simple Block ---------------------------------------------------------------
prtSimpleBlock :: Int -> Int -> SimpleBlock -> String
prtSimpleBlock n m (SimpleBlock label body lastInstr)
  = paste "\n"
    $ [prt label ++ ":"]
      ++ map (prtComSimpleInstr n m) body
      ++ [prtComBranchInstr n m lastInstr]



-- Functions ------------------------------------------------------------------
prtFunc :: Int -> Int -> Func t ts -> String
prtFunc n m (Func retT paramTs params funcId blocks)
  = paste "\n" 
    $ [ paste " " [ "define"
                  , prt retT
                  , prt funcId ++ "(" ++ prtParams paramTs params ++ ")"
                  , "{"
                  ]
      ]
      ++ map (prtSimpleBlock n m) blocks
      ++ ["}"]
        
    where
      prtParams :: SList ts -> ParamList ts -> String
      prtParams SNil DNil = ""
      prtParams (SCons t SNil) (arg :> DNil) = paste " " [prt t, prt arg]
      prtParams (SCons t ts) (arg :> args)
        = paste " " [prt t, prt arg] ++ ", " ++ prtParams ts args



-- Program --------------------------------------------------------------------

prtProg :: Int -> Int -> LLVMProg -> String
prtProg n m LLVM  { mainFunc    = mainFunc
                  , funcs       = funcs
                  , externFuncs = externFuncs
                  , strLits     = strLits
                  
                  , customTs    = customTs
                  , mallocTs    = mallocTs
                  , arrTs       = arrTs
                  }
  = paste "\n\n" 
    $ [prtExternFuncs externFuncs, prtStrLits strLits]
    ++ ["; -- array structs --------------------------------------------------"]
    ++ map prtSomeArrStructDef arrTs
    ++ ["; -- custom type definitions ----------------------------------------"]
    ++ map prtSomeClassDef customTs
    ++ ["; -- methods --------------------------------------------------------"]
    ++ map (prtSomeMethods n m) customTs
    ++ ["; -- malloc functions -----------------------------------------------"]
    ++ map (prtSomeMallocFunc n) mallocTs
    ++ ["; -- newArr functions -----------------------------------------------"]
    ++ map (prtSomeNewArrFunc n) arrTs
    ++ ["; -- user defined functions -----------------------------------------"]
    ++ [prtFunc n m mainFunc]
    ++ map (prtSomeFunc n) funcs

    where
      prtSomeFunc :: Int -> SomeFunc -> String
      prtSomeFunc n (_ :&&: func) = prtFunc n m func

      prtSomeMallocFunc :: Int -> Some SPrimType -> String
      prtSomeMallocFunc n (Some t) = prtMallocFunc n t

      prtSomeNewArrFunc :: Int -> Some SPrimType -> String
      prtSomeNewArrFunc n (Some t) = prtNewArrFunc n t

      prtSomeArrStructDef :: Some SPrimType -> String
      prtSomeArrStructDef (Some t) = prtArrStructDef t

      prtSomeClassDef :: Some StructDef -> String
      prtSomeClassDef (Some def) = prtClassDef def

      prtSomeMethods :: Int -> Int -> Some StructDef -> String
      prtSomeMethods n m (Some def) = prtClassMethods n m def

      prtExternFuncs :: [SomeFuncLabel] -> String
      prtExternFuncs funcs = paste "\n" (map prtExternFunc funcs)
      prtExternFunc :: SomeFuncLabel -> String
      prtExternFunc ((t, ts) :&&: funcLabel)
        = paste " " [ "declare"
                    , prt t
                    , prt funcLabel ++ "(" ++ sPaste ", " ts ++ ")"
                    ]
      
      prtStrLits :: [(String, SomeStrConst)] -> String
      prtStrLits consts = paste "\n" (map prtStrLit consts)
      prtStrLit :: (String, SomeStrConst) -> String
      prtStrLit (s, n :&: const)
        = paste " " [ prt const
                    , "= internal constant"
                    , prt (SArray i8 n)
                    , prtStr s
                    ]
      
      prtStr s = "c\"" ++ prtS s ++ "\\00\""
      prtS ""              = ""
      prtS ('\\' : c : s) = "\\" ++ charCode ++ prtS s where
        charCode = case c of
          
          -- based on this
          -- https://en.wikipedia.org/wiki/Escape_sequences_in_C
          'a'   -> "07"
          'b'   -> "08"
          'e'   -> "1B"
          'f'   -> "0C"
          'n'   -> "0A"
          'r'   -> "0D"
          't'   -> "09"
          'v'   -> "0B"
          '\\'  -> "5C"
          '\''  -> "27"
          '"'   -> "22"
          '?'   -> "3F"
          
          _ -> error "unexpected character after '\\'"

      prtS (c : s)         = c : prtS s








-------------------------------------------------------------------------------
mallocFuncName :: SPrimType t -> String
mallocFuncName t = ".malloc." ++ prt' t where
  prt' :: SPrimType t -> String
  prt' t = case t of
    SStruct ss    -> singToString ss
    SArrStruct tt -> mkArrStructName tt
    SArray {}     -> error "INTERNAL ERROR (malloc returning [_ x _]*)"
    SPtr tt       -> prt' tt ++ "ptr"
    _             -> prt t

newArrFuncName :: SPrimType t -> String
newArrFuncName t = ".new" ++ mkArrStructName t

prtMallocFunc :: Int -> SPrimType t -> String
prtMallocFunc n t = paste "\n"
  $ [ "define " ++ prt t ++ "* @" ++ mallocFuncName t ++ "(i32 %n) {"
    , "entry:"
    ]
  ++ map (tab n)
    [ "%size = getelementptr " ++ prt t ++ ", " ++ prt t ++ "* null, i32 1\n"
    , "%sizeI = ptrtoint " ++ prt t ++ "* %size to i32"
    , "%arrSize = mul i32 %sizeI, %n"
    , ""
    , "%ptr = call i1* @malloc(i32 %arrSize)"
    , "%res = bitcast i1* %ptr to " ++ prt t ++ "*"
    , ""
    , "ret " ++ prt t ++ "* %res"
    ]
  ++ [ "}" ]


prtNewArrFunc :: Int -> SPrimType t -> String
prtNewArrFunc n t = paste "\n"
  $ [ "define " ++ arrT ++ "* @" ++ newArrFuncName t ++ "(i32 %n) {"
    , "entry:"
    ] 
  ++ map (tab n)
    [ "%size = getelementptr " ++ arrT ++ ", " ++ arrT ++ "* null, i32 1"
    , "%sizeI = ptrtoint " ++ arrT ++ "* %size to i32"
    , "%ptr = call i1* @malloc(i32 %sizeI)"
    , "%res = bitcast i1* %ptr to " ++ arrT ++ "*"
    , ""
    , "%arrPtr = getelementptr " ++ arrT ++ ", " ++ arrT ++ "* %res, i32 0, i32 0"
    , "%lenPtr = getelementptr " ++ arrT ++ ", " ++ arrT ++ "* %res, i32 0, i32 1"
    , ""
    , "%newArr = call " ++ prt t ++ "* @" ++ mallocFuncName t ++ "(i32 %n)"
    , "store " ++ prt t ++ "* %newArr, " ++ prt t ++ "** %arrPtr"
    , "store i32 %n, i32* %lenPtr"
    , ""
    , "ret " ++ arrT ++ "* %res"
    ]
  ++ [ "}" ]

  where
    arrT = prt (SArrStruct t)

--------------------------------------------------------------------------------
prtArrStructDef :: SPrimType t -> String
prtArrStructDef elemT = paste " " 
  [ prt (SArrStruct elemT)
  , "= type"
  , paste " " ["{", prt (SPtr elemT) ++ ",", prt (sing @(I 32)), "}"]
  ]

--------------------------------------------------------------------------------
prtClassDef :: StructDef t -> String
prtClassDef def = paste "\n" 
    [ prtVtableTypeDef def
    , prtVtableDef def
    , prtStructDef def
    ]

prtClassMethods :: Int -> Int -> StructDef t -> String
prtClassMethods n m def@StructDef
                        { structName  = name
                        , methods     = methods
                        , constructor = constructor
                        , .. }
  = paste "\n\n"
    $ sep
    : map (prtMethod n m) 
      ((constrType :&&: constructor) : getNewMethods methods)

  where
    clsName = singToString name
    sep = "; -- " ++ clsName ++ " " ++ replicate (63 - length clsName) '-'
    constrType = (SVoid, SCons (SPtr $ SStruct name) SNil)
    
    getNewMethods :: [(String, FuncOrParent)] -> [SomeFunc]
    getNewMethods l = foldl filterVal [] l where
      filterVal :: [SomeFunc] -> (String, FuncOrParent) -> [SomeFunc]
      filterVal acc (f, (argTs, retT) :&&: x) = case insertParam2' x of
        Right func  -> acc ++ [(retT, argTs) :&&: func]
        Left _      -> acc

    prtMethod :: Int -> Int -> SomeFunc -> String
    prtMethod n m (_ :&&: (Func retT paramTs params (FuncLabel f) blocks))
      = prtFunc n m (Func retT paramTs params methodLabel blocks)

      where
        methodLabel = FuncLabel $ C.mkMethodName clsName f


prtStructDef :: StructDef t -> String
prtStructDef StructDef { structName = name, attributes = attrs, .. }
  = paste " "
    [ prt (SStruct name)
    , "= type"
    , "{"
    , paste ", "
      $ prt (SPtr $ SVTable name) : map prtSomeType (toList attrs)
    , "}"
    ]

    where
      
      toList :: DList a ts -> [Some a]
      toList DNil = []
      toList (x :> xs) = Some x : toList xs

prtVtableTypeDef :: StructDef t -> String
prtVtableTypeDef StructDef { structName = name, methods = methods, .. }
  = paste " " 
    [ prt (SVTable name)
    , "= type"
    , "{"
    , paste ", " $ map (prtSomeType . ptrToTypeOf) methods
    , "}"
    ]

    where
      ptrToTypeOf :: (String, FuncOrParent) -> Some SPrimType
      ptrToTypeOf (_, (argTs, retT) :&&: x) = case insertParam2' x of
        Left (Some parent) -> 
          Some $ SPtr $ SFuncType retT argTs
        
        Right _-> 
          Some $ SPtr $ SFuncType retT argTs
        
      

prtVtableDef :: StructDef t -> String
prtVtableDef StructDef { structName = name, methods = methods, .. }
  = paste " " 
    --[ prtVtableType (SStruct name)
    [ prt (SpecialConst (C.mkVtableName $ singToString name))
    , "= global"
    , prt (SVTable name)
    , "{"
    , paste ", " $ map prtMethodLabel methods
    , "}"
    ]

    where

      prtMethodLabel :: (String, FuncOrParent) -> String
      prtMethodLabel (method, (argTs, retT) :&&: x) = case insertParam2' x of
        Right {} -> paste " " [fptrType name, mkMethod name method]
        Left (Some cls) ->
            paste " " [fptrType cls, mkMethod cls method]
        
        where
          fptrType :: SStr s -> String
          fptrType cls = prt $ SPtr
            $ SFuncType retT argTs

          mkMethod s method
            = prt $ FuncLabel $ C.mkMethodName (singToString s) method

      



