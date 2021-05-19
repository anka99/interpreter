module Interpreter where

import Prelude
import System.Exit ( exitFailure, exitSuccess, exitWith )

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import Data.Map

import AbsEmm
import LexEmm
import ParEmm
import PrintEmm
import SkelEmm ()

import Eval ( eval, interpretDeclList, evalFunction )
import Turbo
import Memory
import Typechecker ( checkProgramTypes )

type Err        = Either String
type ParseFun a = [Token] -> Err a

interpret :: String -> IO ()
interpret content =
  case parsedContent of
    Left err -> do
      putStrLn err
      exitFailure
    Right tree -> do
      checkProgramTypes tree
      (res, store) <- runTurbo emptyEnv empty (runProgram tree)
      case res of
        Right (env, IntVal val) -> do
          putStrLn $ show val
          exitSuccess
        Left err -> do
          putStrLn err
          exitFailure

  where
  parsedContent = pProgram $ myLexer content

runProgram :: Program -> TurboMonad (Env, Value)
runProgram (Program pos decls) = do
  env <- interpretDeclList decls
  value <- local (const env) $ evalFunction Nothing (Ident "main") []
  return (env, value)
