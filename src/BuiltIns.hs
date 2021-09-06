{-# LANGUAGE
    FlexibleContexts
  , DataKinds
  , TypeApplications
  , GADTs
#-}

module BuiltIns where


import Control.Monad.State hiding (void)
import Control.Monad.Except hiding (void)

import Data.Singletons.Prelude hiding ( Error )
import Data.Singletons.TypeLits hiding ( Error )

import TypeCheck.State ( TypeCheckState, emptyState )
import TypeCheck.StateUtils
import TypeCheck.Declarations
import Position.Position
import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import LLVM.LLVM
import Errors
import LangElemClasses
import Dependent
import LLVM.Print
import SingChar


ident :: String -> S.Ident
ident = S.Ident fakePos

declareBuiltIn :: (MonadState TypeCheckState m, MonadError Error m)
  => m ()

declareBuiltIn = do
  declareFunc (ident "printInt")    STVoid  (SCons STInt SNil)
  declareFunc (ident "printString") STVoid  (SCons STStr SNil)
  declareFunc (ident "error")       STVoid  SNil
  declareFunc (ident "readInt")     STInt   SNil
  declareFunc (ident "readString")  STStr   SNil
  subFuncScope



initTypeCheckState :: TypeCheckState
initTypeCheckState
  = fromRight $ runExcept (execStateT declareBuiltIn emptyState)

  where
    fromRight (Right x) = x
    fromRight (Left _)
      = error "INTERNAL ERROR (built-in functions declaration)"


externFuncLabels :: [SomeFuncLabel]
externFuncLabels
  = [ (SVoid,             sing @'[I 32])      :&&: FuncLabel "printInt"
    , (SVoid,             sing @'[Ptr (I 8)]) :&&: FuncLabel "printString"
    , (SVoid,             sing @'[])          :&&: FuncLabel "error"
    , (sing @(I 32),      sing @'[])          :&&: FuncLabel "readInt"
    , (sing @(Ptr (I 8)), sing @'[])          :&&: FuncLabel "readString"
    
    , (sing @(Ptr (I 8)), sing @'[Ptr (I 8), Ptr (I 8)]) :&&: strConcatLabel
    ]


strConcatLabel :: FuncLabel (Ptr (I 8)) '[Ptr (I 8), Ptr (I 8)]
strConcatLabel = FuncLabel ".strconcat"


mallocLabel :: Sing t -> FuncLabel (Ptr t) '[I 32]
mallocLabel t = FuncLabel $ mallocFuncName t

newArrLabel :: Sing t -> FuncLabel (Ptr (ArrStruct t)) '[I 32]
newArrLabel t = FuncLabel $ newArrFuncName t


