{-# LANGUAGE FlexibleContexts #-}
module GenErrors where

import Control.Monad.Except
import LangElemClasses
import Position.Position (Pos)
import Errors

type PError = Pos -> Error

todoError p = OnePosError p "TODO"

noSuchVarError :: IsIdent i => i -> Pos -> Error
noSuchVarError i p = OnePosError p $ "Variable " ++ name i ++ " does not exist"

noSuchFuncError :: IsIdent i => i -> Pos -> Error
noSuchFuncError i p
  = OnePosError p $ "Function " ++ name i ++ " does not exist"

noSuchClassError :: IsIdent i => i -> Pos -> Error
noSuchClassError i p
  = OnePosError p $ "Class " ++ name i ++ " does not exist"


wrongIdentTypeError :: (IsIdent i, IsType t1, IsType t2)
  => i -> t1 -> t2 -> Pos -> Error
wrongIdentTypeError i expected actual p = OnePosError p 
  $ "Wrong type of variable " ++ name i 
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual
  
wrongVarTypeError :: (IsVar v, IsType t1, IsType t2)
  => v -> t1 -> t2 -> Pos -> Error
wrongVarTypeError v expected actual p = OnePosError p
  $ "Wrong type of value " ++ printVar v
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

wrongLitTypeError :: (IsLit l, IsType t1, IsType t2)
  => l -> t1 -> t2 -> Pos -> Error
wrongLitTypeError l expected actual p = OnePosError p
  $ "Wrong type of literal " ++ printLit l 
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

wrongExprTypeError :: (IsExpr e, IsType t1, IsType t2)
  => e -> t1 -> t2 -> Pos -> Error
wrongExprTypeError e expected actual p = OnePosError p
  $ "Wrong type of expression " ++ printExpr e
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

exprTypeNotInListError :: (IsExpr e, IsType t1, IsType t2)
  => e -> [t1] -> t2 -> Pos -> Error
exprTypeNotInListError e expectedList actual p = OnePosError p
  $ "Wrong type of expression " ++ printExpr e
    ++ ". Expected: " ++ prtList printType " or " expectedList
    ++ ", Actual: " ++ printType actual

notAVarError :: IsIdent i => i -> Pos -> Error
notAVarError id p = OnePosError p $ name id ++ " is not a variable"

noAttributeError :: (IsType cls, IsIdent i) => i -> cls -> Pos -> Error
noAttributeError id cls p = OnePosError p
  $ "Class " ++ printType cls ++ " has no member " ++ name id

noArrAttrError :: IsIdent i => i -> Pos -> Error
noArrAttrError id p = OnePosError p
  $ "Array has no member " ++ name id

noMethodError :: (IsIdent i, IsType t) => i -> t -> Pos -> Error
noMethodError id t p = OnePosError p
  $ "Type " ++ printType t ++ " has no method " ++ name id

noClsMethodError :: (IsIdent i, IsType t) => i -> t -> Pos -> Error
noClsMethodError id t p = OnePosError p
  $ "Class " ++ printType t ++ " has no method " ++ name id

noArrMethodError :: IsIdent i => i -> Pos -> Error
noArrMethodError id p = OnePosError p
  $ "Array has no method " ++ name id

notAnArrayArror :: IsExpr e => e -> Pos -> Error
notAnArrayArror e p = OnePosError p $ printExpr e ++ " is not an array"


wrongOpTypeError :: (IsOp op, IsType t) => op -> t -> Pos -> Error
wrongOpTypeError op t p = OnePosError p
  $ "Values of type " ++ printType t
    ++ " are incompatibile with operator " ++ printOp op

notAFuncError :: IsVar v => v -> Pos -> Error
notAFuncError var p = OnePosError p
  $ printVar var ++ " is not a function"


wrongNOParamsError :: IsVar v => v -> Int -> Int -> Pos -> Error
wrongNOParamsError v expected actual p = OnePosError p
  $ "Function / Method " ++ printVar v 
    ++ " applied to wrong number of parameters."
    ++ ". Expected: " ++ show expected
    ++ ", Actual: " ++ show actual



varAlredyDeclaredError :: IsIdent i => i -> Pos -> Pos -> Error
varAlredyDeclaredError id declaredAt p = OnePosError p
  $ "Variable " ++ name id ++ " alredy declared at " ++ show declaredAt

funcAlredyDeclaredError :: IsIdent i => i -> Pos -> Pos -> Error
funcAlredyDeclaredError id declaredAt p = OnePosError p
  $ "Function " ++ name id ++ " alredy declared at " ++ show declaredAt

classAlredyDeclaredError :: IsIdent i => i -> Pos -> Pos -> Error
classAlredyDeclaredError id declaredAt p = OnePosError p
  $ "Class " ++ name id ++ " alredy declared at " ++ show declaredAt

attrAlredyDeclaredError :: (IsIdent i1, IsIdent i2)
  => i1 -> i2 -> Pos -> Pos -> Error
attrAlredyDeclaredError attrId clsId declaredAt p = OnePosError p
  $ "Attribute " ++ name attrId ++ " of class " ++ name clsId
    ++ " alredy declared at " ++ show declaredAt

methodAlredyDeclaredError :: (IsIdent i1, IsIdent i2)
  => i1 -> i2 -> Pos -> Pos -> Error
methodAlredyDeclaredError methodId clsId declaredAt p = OnePosError p
  $ "Method " ++ name methodId ++ " of class " ++ name clsId
    ++ " alredy declared at " ++ show declaredAt


nestedClassError :: Pos -> Error
nestedClassError p = OnePosError p "Nested classes are not supported"


returnVoidError :: IsType t => t -> Pos -> Error
returnVoidError t p = OnePosError p
  $ "Function returns void while it is expected to return " 
    ++ printType t

voidDeclarationError :: IsIdent i => i -> Pos -> Error
voidDeclarationError id p = OnePosError p
  $ "Attempt to declare a variable " ++ name id ++ " if type void"


selfOutsideClassError :: Pos -> Error
selfOutsideClassError p
  = OnePosError p "keyword `self` is being used utside a class scope"

typesNotCompatibileError :: (IsExpr e1, IsExpr e2, IsType t1, IsType t2)
  => e1 -> e2 -> t1 -> t2 -> Pos -> Error
typesNotCompatibileError e1 e2 t1 t2 p = OnePosError p
  $ " expressions " ++ printExpr e1 ++ " and " ++ printExpr e2
    ++ " are of types " ++ printType t1 ++ " and " ++ printType t2
    ++ ", which are incompatible"

-- INTERNAL -------------------------------------------------------------------

internalNoReturnTypeError :: Pos -> Error
internalNoReturnTypeError p
  = OnePosError p "INTERNAL ERROR (no return type in state)"

internalNoClassError :: Pos -> Error
internalNoClassError p
  = OnePosError p "INTERNAL ERROR (no class with given id)"

internallNullKWError :: Pos -> Error
internallNullKWError p = OnePosError p "INTERNAL ERROR (Null keyword)"

internalClassToFuncDefError :: Pos -> Error
internalClassToFuncDefError p = OnePosError p 
  $ "INTERNAL ERROR (Conversion of class definition to function definition)"

internalFuncToClassDefError :: Pos -> Error
internalFuncToClassDefError p = OnePosError p 
  $ "INTERNAL ERROR (Conversion of function definition to class definition)"

