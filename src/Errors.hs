{-# LANGUAGE FlexibleContexts #-}
module Errors where

import Control.Monad.Except
import LangElemClasses
import Position.Position (Pos)

data Error
  = SimpleError String
  | OnePosError Pos String
  | MultiPosError [Pos] String

instance Show Error where
  show (SimpleError s) = "ERROR: " ++ s
  show (OnePosError p s) = "ERROR at " ++ show p ++ "\n" ++ s
  show (MultiPosError ps s) = "ERRORs at " ++ positions ++ "\n" ++ s
    where
      positions = prtList show  ", " ps
    

prtList :: (a -> String) -> String -> [a] -> String
prtList f _ [] = ""
prtList f sep (x : xs) = foldl (\acc el -> acc ++ sep ++ f el) (f x) xs


throwTODO :: MonadError Error m => m a
throwTODO = throwError $ SimpleError "TODO"

throwTODOP :: MonadError Error m => Pos -> m a
throwTODOP p = throwError $ OnePosError p "TODO"

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
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual
  
wrongVarTypeError :: (IsVar v, IsType t1, IsType t2)
  => Pos-> v -> t1 -> t2 -> Error
wrongVarTypeError p v expected actual = OnePosError p 
  $ "Wrong type of value " ++ printVar v
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

wrongLitTypeError :: (IsLit l, IsType t1, IsType t2)
  => Pos-> l -> t1 -> t2 -> Error
wrongLitTypeError p l expected actual = OnePosError p 
  $ "Wrong type of literal " ++ printLit l 
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

wrongExprTypeError :: (IsExpr e, IsType t1, IsType t2)
  => Pos -> e -> t1 -> t2 -> Error
wrongExprTypeError p e expected actual = OnePosError p
  $ "Wrong type of expression " ++ printExpr e
    ++ ". Expected: " ++ printType expected
    ++ ", Actual: " ++ printType actual

exprTypeNotInListError :: (IsExpr e, IsType t1, IsType t2)
  => Pos -> e -> [t1] -> t2 -> Error
exprTypeNotInListError p e expectedList actual = OnePosError p
  $ "Wrong type of expression " ++ printExpr e
    ++ ". Expected: " ++ prtList printType " or " expectedList
    ++ ", Actual: " ++ printType actual

notAVarError :: IsIdent i => Pos -> i -> Error
notAVarError p id = OnePosError p $ name id ++ " is not a variable"

noAttributeError :: (IsType cls, IsIdent i) => Pos -> cls -> i -> Error
noAttributeError p cls id = OnePosError p
  $ "Class " ++ printType cls ++ " has no member " ++ name id

noArrAttrError :: IsIdent i => Pos -> i -> Error
noArrAttrError p id = OnePosError p
  $ "Array has no member " ++ name id

noMethodError :: (IsIdent i, IsType t) => Pos -> t -> i -> Error
noMethodError p t id = OnePosError p
  $ "Type " ++ printType t ++ " has no method " ++ name id

noClsMethodError :: (IsIdent i, IsType t) => Pos -> t -> i -> Error
noClsMethodError p t id = OnePosError p
  $ "Class " ++ printType t ++ " has no method " ++ name id

noArrMethodError :: IsIdent i => Pos -> i -> Error
noArrMethodError p id = OnePosError p
  $ "Array has no method " ++ name id

notAnArrayArror :: IsExpr e => Pos -> e -> Error
notAnArrayArror p e = OnePosError p $ printExpr e ++ " is not an array"


wrongOpTypeError :: (IsOp op, IsType t) => Pos -> op -> t -> Error
wrongOpTypeError p op t = OnePosError p
  $ "Values of type " ++ printType t
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


returnVoidError :: IsType t => Pos -> t -> Error
returnVoidError p t = OnePosError p
  $ "Function returns void while it is expected to return " 
    ++ printType t

voidDeclarationError :: Pos -> Error
voidDeclarationError p = OnePosError p
  $ "Attempt to declare a variable of type void"


selfOutsideClassError :: Pos -> Error
selfOutsideClassError p
  = OnePosError p "Keyword `self` is being used utside a class scope"

typesNotCompatibileError :: (IsExpr e1, IsExpr e2, IsType t1, IsType t2)
  => Pos -> e1 -> e2 -> t1 -> t2 -> Error
typesNotCompatibileError p e1 e2 t1 t2 = OnePosError p
  $ "Expressions " ++ printExpr e1 ++ " and " ++ printExpr e2
    ++ " are of types " ++ printType t1 ++ " and " ++ printType t2
    ++ ", which are incompatible"



relationNotSupportedError :: (IsOp op, IsType t) => Pos -> op -> t -> Error
relationNotSupportedError p op t = OnePosError p
  $ "Relation " ++ printOp op
    ++ " is not supported for values of type " ++ printType t


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

internalNoSuchBlockError :: Error
internalNoSuchBlockError = SimpleError "INTERNAL ERROR (no block info)"

internalBlockAlredyExistsError :: Error
internalBlockAlredyExistsError
  = SimpleError "INTERNAL ERROR (block label repeated)"

internalNoSuchInputError :: Error
internalNoSuchInputError = SimpleError
  $ "INTERNAL ERROR (attempt to add phi instruction with invalid label)"

internalInputNotIncludedError :: Error
internalInputNotIncludedError 
  = SimpleError "INTERNAL ERROR (an input not included in a phi instruction)"

internalScopeLevelBelowZeroError :: Error
internalScopeLevelBelowZeroError
  = SimpleError "INTERNAL ERROR (scope level below 0)"


internalPhiNotPartOfInheritedError :: Error
internalPhiNotPartOfInheritedError
  = SimpleError "INTERNAL ERROR (phi takes uninherited values)"


