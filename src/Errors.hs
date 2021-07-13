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


wrongTypeError :: (IsIdent i, IsType t1, IsType t2)
  => Pos-> i -> t1 -> t2 -> Error
wrongTypeError p i expected actual = OnePosError p 
  $ "Wrong type of variable " ++ name i 
    ++ ". Expected: " ++ toStr expected
    ++ ", Actual: " ++ toStr actual


