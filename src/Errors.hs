module Errors where

import LangElemClasses

type Pos = (Int, Int)
data Error
  = SimpleError String
  | OnePosError Pos String
  | MultiPosError [Pos] String

noSuchVarError :: IsIdent i => Pos -> i -> Error
noSuchVarError p i = OnePosError p $ "Variable " ++ name i ++ " does not exist"

noSuchFuncError :: IsIdent i => Pos -> i -> Error
noSuchFuncError p i
  = OnePosError p $ "Function " ++ name i ++ " does not exist"

noSuchClassError :: IsIdent i => Pos -> i -> Error
noSuchClassError p i
  = OnePosError p $ "Class " ++ name i ++ " does not exist"


wrongVarTypeError :: (IsIdent i, IsType t1, IsType t2)
  => Pos-> i -> t1 -> t2 -> Error
wrongVarTypeError p i expected actual = OnePosError p 
  $ "Wrong type of variable " ++ name i 
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

notAnArrayArror :: IsVar v => Pos -> v -> Error
notAnArrayArror p v = OnePosError p $ printVar v ++ " is not an array"