module Main where

import System.Environment (getArgs)
import System.IO
import Control.Monad

import FromBNFC.ParLatte (myLexer, pProgram)
import FromBNFC.AbsLatte (Program)
import FromBNFC.ErrM

import qualified FromBNFC.TestLatte as TL

parse :: String -> Err Program
parse = pProgram . myLexer

getExitCode :: Err a -> Int
getExitCode (Ok _) = 0
getExitCode (Bad _) = 1

getOutput :: Err a -> String
getOutput (Ok _) = "OK\n"
getOutput (Bad s) = "ERROR\n" ++ s

isBad :: Err a -> Bool
isBad (Bad _) = True
isBad (Ok _) = False


main :: IO Int
main = do

  args <- getArgs
  
  let filename = head args
  
  fileCts <- readFile filename

  let result = parse fileCts

  when (isBad result) TL.main

  let exitCode = getExitCode result
  let output = getOutput result

  hPutStrLn stderr output

  return exitCode

