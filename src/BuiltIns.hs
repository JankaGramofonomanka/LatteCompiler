{-# LANGUAGE FlexibleContexts #-}

module BuiltIns where


import Control.Monad.State hiding (void)
import Control.Monad.Except hiding (void)

import Data.Singletons.Prelude hiding ( Error )

import TypeCheck.State ( TypeCheckState, emptyState )
import TypeCheck.StateUtils
import TypeCheck.Declarations
import Position.Position
import qualified Syntax.Syntax as S
import Syntax.SyntaxDep
import Errors
import LangElemClasses


ident :: String -> S.Ident
ident = S.Ident fakePos

declareBuiltIn :: (MonadState TypeCheckState m, MonadError Error m)
  => m ()

declareBuiltIn = do
  declareFunc (ident "printInt") STVoid (SCons STInt SNil)
  declareFunc (ident "printString") STVoid (SCons STStr SNil)
  declareFunc (ident "error") STVoid SNil
  declareFunc (ident "readInt") STInt SNil
  declareFunc (ident "readString") STStr SNil
  subFuncScope



initTypeCheckState :: TypeCheckState
initTypeCheckState
  = fromRight $ runExcept (execStateT declareBuiltIn emptyState)

  where
    fromRight (Right x) = x
    fromRight (Left _)
      = error "INTERNAL ERROR (built-in functions declaration)"




