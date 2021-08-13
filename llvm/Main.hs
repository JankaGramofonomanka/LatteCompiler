{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment (getArgs)
import System.IO
import System.IO.Error
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except
import System.FilePath.Posix

import FromBNFC.ParLatte (myLexer, pProgram)
import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.ErrM

import Program

import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS

import TypeCheck.TypeCheck ( ToBeTypeChecked(typeCheck) )
import Syntax.Debloater ( ToBeDebloated(debloat) )

import qualified LLVM.LLVM as LLVM
import LLVM.Convert
import LLVM.State ( emptyState )
import LLVM.Print

import Errors
import BuiltIns ( initTypeCheckState )



getOutput :: Show a => (b -> String) -> Either a b -> String
getOutput prt (Right result) = prt result
getOutput prt (Left err) = "ERROR\n" ++ show err

processFileContents :: String -> IO LLVM.LLVMProg
processFileContents fileCts = case parse fileCts of
  Bad s -> ioError $ userError s

  Ok bloatedAbsTree -> do
      
    let result = runExcept (processTree (debloat bloatedAbsTree))
    case result of
      Left err -> ioError $ userError $ show err
      Right prog -> return prog
  


main :: IO ()
main = do

  args <- getArgs
  
  let filename = head args
  
  fileCts <- readFile filename

  llvm <- processFileContents fileCts

  let outputPath = addExtension (dropExtension filename) "ll"
  writeFile outputPath $ prtProg 8 llvm


