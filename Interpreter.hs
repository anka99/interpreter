-- Program to test parser, automatically generated by BNF Converter.

module Interpreter where

import System.Exit        ( exitFailure, exitSuccess )
import Control.Monad      ( when )

import AbsEmm   ()
import LexEmm   ( Token )
import ParEmm   ( pProgram, myLexer )
import PrintEmm ( Print, printTree )
import SkelEmm  ()

type Err        = Either String
type ParseFun a = [Token] -> Err a

interpret :: String -> IO ()
interpret = putStrLn