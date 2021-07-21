{-# LANGUAGE GADTs #-}

module Optimization.EvalConstants where


import Syntax.SyntaxGADT


evalConstant :: Expr a -> Maybe a
evalConstant expr = case expr of
  EVar      _ var         -> Nothing
  ELitInt   _ (SInt _ i)  -> Just i
  ELitBool  _ b           -> Just b
  EApp      {}            -> Nothing
  EString   _ (SStr _ s)  -> Just s
  Neg       _ e -> do
    val <- evalConstant e
    return (-val)

  Not       _ e -> do
    val <- evalConstant e
    return (not val)

  EOp       _ op lhs rhs -> do
    valLHS <- evalConstant lhs
    valRHS <- evalConstant rhs
    let realOp = getBinOp op
    return $ realOp valLHS valRHS

  ERel      _ op lhs rhs -> do
    valLHS <- evalConstant lhs
    valRHS <- evalConstant rhs
    let realOp = getRelOp op
    return $ realOp valLHS valRHS

  EBool     _ op lhs rhs -> case op of
    And _ -> do
      valLHS <- evalConstant lhs
      if not valLHS then
        return False
      else do
        valRHS <- evalConstant rhs
        return $ valLHS && valRHS
    
    Or _ -> do
      valLHS <- evalConstant lhs
      if valLHS then
        return True
      else do
        valRHS <- evalConstant rhs
        return $ valLHS || valRHS

  NewArr    {} -> Nothing
  NewObj    {} -> Nothing
  Cast      {} -> Nothing

  Concat    _ lhs rhs -> do
    valLHS <- evalConstant lhs
    valRHS <- evalConstant rhs
    return $ valLHS ++ valRHS


--evalBoolConst :: Expr a -> Maybe Bool
--evalBoolConst expr = case expr of
  


getBinOp :: BinOp -> (Int -> Int -> Int)
getBinOp (Plus  _) = (+)
getBinOp (Minus _) = (-)
getBinOp (Times _) = (*)
getBinOp (Div   _) = div
getBinOp (Mod   _) = mod

getRelOp :: Eq a => RelOp a -> (a -> a -> Bool)
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






