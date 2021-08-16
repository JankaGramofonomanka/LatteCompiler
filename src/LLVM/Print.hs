{-# LANGUAGE
    GADTs
  , ScopedTypeVariables
  , DataKinds
  , KindSignatures
  , PolyKinds
  , RankNTypes
#-}

module LLVM.Print where

import Prelude hiding ( EQ )
import Data.Singletons.Sigma
import Data.Singletons.TypeLits
import Data.Singletons.Prelude hiding ( SGT, SLT )

import LLVM.LLVM
import Dependent
import Data.Data (mkConstr)


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


-- Primitive Types ------------------------------------------------------------


printSNat :: SNat n -> String
printSNat n = drop 6 (show n)

fromSNatural :: SNatural z -> Integer
fromSNatural SZero = 0
fromSNatural (SSucc n) = fromSNatural n + 1

printSNatural n = show (fromSNatural n)

instance SimplePrint (SPrimType t) where
  prt (SI n)        = "i" ++ printSNat n
  prt SVoid         = "void"
  prt (SPtr t)      = prt t ++ "*"
  prt (SArray t n)  = "[" ++ printSNatural n ++ " x " ++ prt t ++ "]"


-- Simple Values --------------------------------------------------------------
instance SimplePrint (Reg t) where
  prt (Reg s i) = "%" ++ s ++ "." ++  show i

instance SimplePrint (Constant t) where
  prt (Const s i) = "@" ++ s ++ "." ++ show i

instance SimplePrint (Value t) where
  prt (Var reg) = prt reg
  prt (ILit i) = show i
  prt (BoolLit True) = "true"
  prt (BoolLit False) = "false"
  prt (ConstPtr cst) = prt cst


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
      
  prt (GetElemPtr contT indexT container index) = paste " " 
    [ "getelementptr"
    , prt contT ++ ","
    , prt (SPtr contT)
    , prt container ++ ","
    , prt indexT
    , prt index
    ]
  
  prt (GetArrElemPtr contT indexT1 indexT2 container index1 index2) = paste " " 
    [ "getelementptr"
    , prt contT ++ ","
    , prt (SPtr contT)
    , prt container ++ ","
    , prt indexT1
    , prt index1 ++ ","
    , prt indexT2
    , prt index2
    ]

  prt (ICMP t kind lhs rhs)
    = paste " " ["icmp", prt kind, prt t, prt lhs ++ ",", prt rhs]

  prt (Phi t vals)
    = paste " " ["phi", prt t, paste ", " (map prtValTuple vals)]

    where
      prtValTuple (label, val)
        = "[" ++ prt val ++ ", " ++ prtVarLabel label ++ "]"


-- Instructions ---------------------------------------------------------------

instance SimplePrint SimpleInstr where
  prt (Ass reg expr) = paste " " [prt reg, "=", prt expr]
  prt (VoidExpr expr) = prt expr

prtSimpleInstr :: Int -> SimpleInstr -> String
prtSimpleInstr n instr = replicate n ' ' ++ prt instr

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
prtBranchInstr n instr = replicate n ' ' ++ prt instr



-- Simple Block ---------------------------------------------------------------
prtSimpleBlock :: Int -> SimpleBlock -> String
prtSimpleBlock n (SimpleBlock label body lastInstr)
  = paste "\n"
    $ [prt label ++ ":"]
      ++ map (prtSimpleInstr n) body
      ++ [prtBranchInstr n lastInstr]



-- Functions ------------------------------------------------------------------
prtFunc :: Int -> Func t ts -> String
prtFunc n (Func retT paramTs params funcId blocks)
  = paste "\n" 
    $ [ paste " " [ "define"
                  , prt retT
                  , prt funcId ++ "(" ++ prtParams paramTs params ++ ")"
                  , "{"
                  ]
      ]
      ++ map (prtSimpleBlock n) blocks
      ++ ["}"]
        
    where
      prtParams :: SList ts -> ParamList ts -> String
      prtParams SNil DNil = ""
      prtParams (SCons t SNil) (arg :> DNil) = paste " " [prt t, prt arg]
      prtParams (SCons t ts) (arg :> args)
        = paste " " [prt t, prt arg] ++ ", " ++ prtParams ts args



-- Program --------------------------------------------------------------------

prtProg :: Int -> LLVMProg -> String
prtProg n (LLVM mainFunc funcs externFuncs strLits)
  = paste "\n\n" 
    $ [prtExternFuncs externFuncs, prtStrLits strLits, prtFunc n mainFunc]
    ++ map (prtSomeFunc n) funcs

    where
      prtSomeFunc :: Int -> SomeFunc -> String
      prtSomeFunc n (_ :&&: func) = prtFunc n func

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
                    , prt (SArray i8 n), "c\"" ++ s ++ "\\00\""
                    ]







