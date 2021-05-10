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

import Eval ( eval, interpretDeclList )
import Turbo

type Err        = Either String
type ParseFun a = [Token] -> Err a

interpret :: String -> IO ()
interpret content =
  case parsedContent of
    Left err -> do
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      (env, store) <- runTurbo empty empty (runProgram tree)
      putStrLn $ show store
      putStrLn $ show env
      -- exitWith interpretProgram tree
  where
  parsedContent = pProgram $ myLexer content

runProgram :: Program -> TurboMonad (Env, Store)
runProgram (Program pos decls) = do
  env <- interpretDeclList decls
  store <- get
  return (env, store)
