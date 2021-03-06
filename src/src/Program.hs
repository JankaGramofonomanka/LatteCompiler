{-# LANGUAGE FlexibleContexts #-}

module Program where

import System.Environment (getArgs)
import System.IO
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import System.FilePath.Posix

import FromBNFC.ParLatte (myLexer, pProgram)
import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.ErrM

import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS

import TypeCheck.TypeCheck ( ToBeTypeChecked(typeCheck) )
import TypeCheck.CheckReturns
import Syntax.Debloater ( ToBeDebloated(debloat) )
import Optimization.EvalConstants

import qualified LLVM.LLVM as LLVM
import LLVM.Convert
import LLVM.State ( emptyState )
import LLVM.Print

import Errors
import BuiltIns ( initTypeCheckState )
import Optimization.PhiElimination

parse :: String -> Err BNFC.Program
parse = pProgram . myLexer

toEither :: Err a -> Either String a
toEither (Ok x) = Right x
toEither (Bad s) = Left s

success :: Either a b -> Bool
success (Right _) = True
success (Left _) = False

processTree :: MonadError Error m => S.Program -> m LLVM.LLVMProg
processTree tree = do
  typeChecked <- evalStateT (typeCheck tree) initTypeCheckState
  progWithReturns <- checkReturns $ evalConstants typeChecked
  prog <- evalStateT (addProg progWithReturns >> extractLLVM) emptyState
  return $ eliminatePhisProg prog
  
  






