module Main where

import System.Environment (getArgs)
import System.IO
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except

import FromBNFC.ParLatte (myLexer, pProgram)
import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.ErrM

import Program
import Syntax.Debloater
import LLVM.Print
import Errors




getExitCode :: Either a b -> Int
getExitCode (Right _) = 0
getExitCode (Left _) = 1

getOutput :: Show a => Either a b -> String
getOutput (Right _) = "OK\n"
getOutput (Left err) = "ERROR\n" ++ show err


processFileContents :: String -> (Int, String)
processFileContents fileCts = case parse fileCts of
  Bad s -> (getExitCode $ Left s, getOutput $ Left s)

  Ok bloatedAbsTree -> (exitCode, output) where
      
    result = runExcept (processTreeTemp (debloat bloatedAbsTree))

    exitCode = getExitCode result
    output = getOutput result




main :: IO Int
main = do

  args <- getArgs
  
  let filename = head args
  
  fileCts <- readFile filename

  let (exitCode, output) = processFileContents fileCts

  hPutStrLn stderr output

  return exitCode

