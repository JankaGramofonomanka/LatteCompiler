{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import LangElemClasses
import Position (Pos)

data Error
  = SimpleError String
  | OnePosError Pos String
  | MultiPosError [Pos] String


throwTODO :: MonadError Error m => m a
throwTODO = throwError $ SimpleError "TODO"

noSuchVarError :: IsIdent i => Pos -> i -> Error
noSuchVarError p i = OnePosError p $ "Variable " ++ name i ++ " does not exist"

noSuchFuncError :: IsIdent i => Pos -> i -> Error
noSuchFuncError p i
  = OnePosError p $ "Function " ++ name i ++ " does not exist"

noSuchClassError :: IsIdent i => Pos -> i -> Error
noSuchClassError p i
  = OnePosError p $ "Class " ++ name i ++ " does not exist"


wrongIdentTypeError :: (IsIdent i, IsType t1, IsType t2)
  => Pos-> i -> t1 -> t2 -> Error
wrongIdentTypeError p i expected actual = OnePosError p 
  $ "Wrong type of variable " ++ name i 
    ++ ". Expected: " ++ toStr expected
    ++ ", Actual: " ++ toStr actual
  
wrongVarTypeError :: (IsVar v, IsType t1, IsType t2)
  => Pos-> v -> t1 -> t2 -> Error
wrongVarTypeError p v expected actual = OnePosError p 
  $ "Wrong type of value " ++ printVar v
    ++ ". Expected: " ++ toStr expected
    ++ ", Actual: " ++ toStr actual

wrongLitTypeError :: (IsLit l, IsType t1, IsType t2)
  => Pos-> l -> t1 -> t2 -> Error
wrongLitTypeError p l expected actual = OnePosError p 
  $ "Wrong type of literal " ++ printLit l 
    ++ ". Expected: " ++ toStr expected
    ++ ", Actual: " ++ toStr actual

wrongExprType :: (IsExpr e, IsType t1, IsType t2)
  => Pos -> e -> t1 -> t2 -> Error
wrongExprType p e expected actual = OnePosError p
  $ "Wrong type of expression"
    ++ ". Expected: " ++ toStr expected
    ++ ", Actual: " ++ toStr actual

notAVarError :: IsIdent i => Pos -> i -> Error
notAVarError p id = OnePosError p $ name id ++ " is not a variable"

noAttributeError :: (IsType cls, IsIdent i) => Pos -> cls -> i -> Error
noAttributeError p cls id = OnePosError p
  $ "Class " ++ toStr cls ++ " has no member " ++ name id

noArrAttrError :: IsIdent i => Pos -> i -> Error
noArrAttrError p id = OnePosError p
  $ "Array has no member " ++ name id

noMethodError :: (IsIdent i, IsType t) => Pos -> t -> i -> Error
noMethodError p t id = OnePosError p
  $ "Type " ++ toStr t ++ " has no method " ++ name id

noClsMethodError :: (IsIdent i, IsType t) => Pos -> t -> i -> Error
noClsMethodError p t id = OnePosError p
  $ "Class " ++ toStr t ++ " has no method " ++ name id

noArrMethodError :: IsIdent i => Pos -> i -> Error
noArrMethodError p id = OnePosError p
  $ "Array has no method " ++ name id

notAnArrayArror :: IsVar v => Pos -> v -> Error
notAnArrayArror p v = OnePosError p $ printVar v ++ " is not an array"


wrongOpTypeError :: (IsOp op, IsType t) => Pos -> op -> t -> Error
wrongOpTypeError p op t = OnePosError p
  $ "Values of type " ++ toStr t
    ++ " are incompatibile with operator " ++ printOp op

notAFuncError :: IsVar v => Pos -> v -> Error
notAFuncError p var = OnePosError p
  $ printVar var ++ " is not a function"


wrongNOParamsError :: IsVar v => Pos -> v -> Int -> Int -> Error
wrongNOParamsError p v expected actual = OnePosError p
  $ "Function / Method " ++ printVar v 
    ++ " applied to wrong number of parameters."
    ++ ". Expected: " ++ show expected
    ++ ", Actual: " ++ show actual



varAlredyDeclaredError :: IsIdent i => Pos -> i -> Pos -> Error
varAlredyDeclaredError p id declaredAt = OnePosError p
  $ "Variable " ++ name id ++ " alredy declared at " ++ show declaredAt

funcAlredyDeclaredError :: IsIdent i => Pos -> i -> Pos -> Error
funcAlredyDeclaredError p id declaredAt = OnePosError p
  $ "Function " ++ name id ++ " alredy declared at " ++ show declaredAt

classAlredyDeclaredError :: IsIdent i => Pos -> i -> Pos -> Error
classAlredyDeclaredError p id declaredAt = OnePosError p
  $ "Class " ++ name id ++ " alredy declared at " ++ show declaredAt

attrAlredyDeclaredError :: (IsIdent i1, IsIdent i2)
  => Pos -> i1 -> i2 -> Pos -> Error
attrAlredyDeclaredError p attrId clsId declaredAt = OnePosError p
  $ "Attribute " ++ name attrId ++ " of class " ++ name clsId
    ++ " alredy declared at " ++ show declaredAt

methodAlredyDeclaredError :: (IsIdent i1, IsIdent i2)
  => Pos -> i1 -> i2 -> Pos -> Error
methodAlredyDeclaredError p methodId clsId declaredAt = OnePosError p
  $ "Method " ++ name methodId ++ " of class " ++ name clsId
    ++ " alredy declared at " ++ show declaredAt


nestedClassError :: Pos -> Error
nestedClassError p = OnePosError p "Nested classes are not supported"

