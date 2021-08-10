{-# LANGUAGE 
    GADTs
  , KindSignatures
  , DataKinds
  , TypeFamilies
  , FlexibleContexts
  , StandaloneKindSignatures
#-}

module Optimization.EvalConstants where


import Syntax.SyntaxDep
import Data.Kind





type Const :: LatteType -> Type
data Const t where
  IConst :: Int -> Const TInt
  BConst :: Bool -> Const TBool
  SConst :: String -> Const TStr

instance Eq (Const t) where
  IConst x == IConst y = x == y
  BConst x == BConst y = x == y
  SConst x == SConst y = x == y

instance Ord (Const t) where
  IConst x <= IConst y = x <= y
  BConst x <= BConst y = x <= y
  SConst x <= SConst y = x <= y

evalConstant :: Expr a -> Maybe (Const a)
evalConstant expr = case expr of
  EVar      _ _ var -> Nothing
  ELitInt   _ i     -> Just $ IConst i
  ELitBool  _ b     -> Just $ BConst b
  EApp      {}      -> Nothing
  EString   _ s     -> Just $ SConst s
  Neg       _ e -> do
    IConst val <- evalConstant e
    return $ IConst (-val)

  Not       _ e -> do
    BConst val <- evalConstant e
    return $ BConst $ not val

  EOp       _ op lhs rhs -> do
    IConst valLHS <- evalConstant lhs
    IConst valRHS <- evalConstant rhs
    let realOp = getBinOp op
    return $ IConst $ realOp valLHS valRHS

  ERel      _ _ op lhs rhs -> do
    valLHS <- evalConstant lhs
    valRHS <- evalConstant rhs
    let realOp = getRelOp op
    return $ BConst $ realOp valLHS valRHS

  EBool     _ op lhs rhs -> case op of
    And _ -> do
      BConst valLHS <- evalConstant lhs
      if not valLHS then
        return $ BConst False
      else do
        BConst valRHS <- evalConstant rhs
        return $ BConst $ valLHS && valRHS
    
    Or _ -> do
      BConst valLHS <- evalConstant lhs
      if valLHS then
        return $ BConst True
      else do
        BConst valRHS <- evalConstant rhs
        return $ BConst $ valLHS || valRHS

  NewArr    {} -> Nothing
  NewObj    {} -> Nothing
  Cast      {} -> Nothing

  Concat    _ lhs rhs -> do
    SConst valLHS <- evalConstant lhs
    SConst valRHS <- evalConstant rhs
    return $ SConst $ valLHS ++ valRHS




getBinOp :: BinOp -> (Int -> Int -> Int)
getBinOp (Plus  _) = (+)
getBinOp (Minus _) = (-)
getBinOp (Times _) = (*)
getBinOp (Div   _) = div
getBinOp (Mod   _) = mod

getRelOp :: RelOp a -> (Const a -> Const a -> Bool)
getRelOp op = case op of
  LTH _ -> (<)
  LE  _ -> (<=)
  GTH _ -> (>)
  GE  _ -> (>=)
  EQU _ -> (==)
  NE  _ -> (/=)


getBoolOp :: BoolOp -> (Bool -> Bool -> Bool)
getBoolOp (And  _) = (&&)
getBoolOp (Or   _) = (||)






