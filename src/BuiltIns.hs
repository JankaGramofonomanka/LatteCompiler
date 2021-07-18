{-# LANGUAGE FlexibleContexts #-}

module BuiltIns where


import Control.Monad.State hiding (void)
import Control.Monad.Except hiding (void)

import TypeCheck.State ( TypeCheckState, emptyState )
import TypeCheck.PuttersAndDeclarations
import Position.Position
import qualified Syntax.Syntax as S
import Errors
import LangElemClasses

_p :: Pos
_p = fakePos


int :: S.Type
int = S.Int _p
str :: S.Type
str = S.Str _p
bool :: S.Type
bool = S.Bool _p
void :: S.Type
void = S.Void _p

ident :: String -> S.Ident
ident = S.Ident _p

declareBuiltIn :: (MonadState TypeCheckState m, MonadError Error m)
  => m ()

declareBuiltIn = do
  declareFunc (ident "printInt") void [int]
  declareFunc (ident "printString") void [str]
  declareFunc (ident "error") void ([] :: [S.Type])
  declareFunc (ident "readInt") int ([] :: [S.Type])
  declareFunc (ident "readString") str ([] :: [S.Type])
  subFuncScope



initTypeCheckState :: TypeCheckState
initTypeCheckState
  = fromRight $ runExcept (execStateT declareBuiltIn emptyState)

  where
    fromRight (Right x) = x
    fromRight (Left _)
      = error "INTERNAL ERROR (built-in functions declaration)"




