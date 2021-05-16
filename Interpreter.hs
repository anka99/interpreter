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

type Err        = Either String
type ParseFun a = [Token] -> Err a

interpret :: String -> IO ()
interpret content =
  case parsedContent of
    Left err -> do
      putStrLn err
      exitFailure
    Right tree -> do
      (res, store) <- runTurbo emptyEnv empty (runProgram tree)
      case res of
        Right (env, val) -> do
          -- putStrLn $ show store
          -- putStrLn $ show env
          putStrLn $ show val
        Left err -> putStrLn err
      -- exitWith interpretProgram tree
  where
  parsedContent = pProgram $ myLexer content

runProgram :: Program -> TurboMonad (Env, Value)
runProgram (Program pos decls) = do
  env <- interpretDeclList decls
  value <- local (changeEnvTo env) $ evalFunction Nothing (Ident "main") []
  return (env, value)
