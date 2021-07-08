module Main where

import System.Environment (getArgs)
import System.IO



  

main :: IO Int
main = do

  args <- getArgs
  
  let filename = head args
  
  fileCts <- readFile filename


  let okExitCode = 0
  let okOutput = "OK\n"

  let errExitCode = 1
  let errOutput = "ERROR\n fake error" 


  hPutStrLn stderr okOutput

  return okExitCode

