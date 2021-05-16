module Err where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Turbo
import AbsEmm

errorPos :: Position -> String -> String
errorPos (Just (line, col)) msg =
  "Error in line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg

errorPos Nothing _ = "Function main not found."

data ErrType
  = TypeErr String
  | MulDecl String
  | Undecl String
  | Uninit
  | NoRet
  | ArgNum Ident Integer Integer
  | NotFun String
  | NotRef
    deriving (Show)

errorMsg :: ErrType -> String
errorMsg (TypeErr s) = "Mismatching type. Expected " ++ s
errorMsg (MulDecl s) = "Multiple declaration of " ++ s
errorMsg (Undecl s) = "Undeclared variable " ++ s
errorMsg Uninit = "Uninitialized value"
errorMsg NoRet = "Function returns no value"
errorMsg (ArgNum ident i1 i2) =
  "Invalid number of arguments for function " ++ show ident ++ ": " ++ show i1 ++
  ".Expected " ++ show i2
errorMsg (NotFun s) = "Not a function: " ++ s;
errorMsg NotRef  = "Not a variable name"
