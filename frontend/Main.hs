module Main where

import System.Environment (getArgs)
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import FromBNFC.ParLatte (myLexer, pProgram)
import qualified FromBNFC.AbsLatte as BNFC
import FromBNFC.ErrM

import qualified Syntax.Syntax as S
import qualified Syntax.SyntaxDep as DS
import TypeCheck.TypeCheck ( ToBeTypeChecked(typeCheck) )
import TypeCheck.State ( emptyState )
import Syntax.Debloater ( ToBeDebloated(debloat) )
import Errors
import BuiltIns ( initTypeCheckState )

parse :: String -> Err BNFC.Program
parse = pProgram . myLexer

toEither :: Err a -> Either String a
toEither (Ok x) = Right x
toEither (Bad s) = Left s

getExitCode :: Either a b -> Int
getExitCode (Right _) = 0
getExitCode (Left _) = 1

getOutput :: Show a => Either a b -> String
getOutput (Right _) = "OK\n"
getOutput (Left err) = "ERROR\n" ++ show err

success :: Either a b -> Bool
success (Right _) = True
success (Left _) = False

processFileContents :: String -> IO (Int, String)
processFileContents fileCts = case parse fileCts of
  Bad s -> return (getExitCode $ Left s, getOutput $ Left s)

  Ok bloatedAbsTree -> do

    let absTree = debloat bloatedAbsTree :: S.Program
    let result = runExcept (evalStateT (typeCheck absTree) initTypeCheckState)

    let exitCode = getExitCode (result :: Either Error DS.Program)
    let output = getOutput result

    return (exitCode, output)



main :: IO Int
main = do

  args <- getArgs
  
  let filename = head args
  
  fileCts <- readFile filename

  (exitCode, output) <- processFileContents fileCts

  hPutStrLn stderr output

  return exitCode

