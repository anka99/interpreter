module Err where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Turbo
import AbsEmm

errorPosR :: Position -> String -> String
errorPosR p s = "Runtime error" ++ errorP p s

errorPos :: Position -> String -> String
errorPos p s = "Error" ++ errorP p s

errorP :: Position -> String -> String
errorP (Just (line, col)) msg =
  " in line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg

errorP Nothing _ = " : Function main not found."

data ErrType
  = TypeErr String
  | MulDecl String
  | Undecl String
  | Uninit
  | NoRet
  | ArgNum Ident Integer Integer
  | NotFun String
  | NotRef
  | DiffRets
  | CntErr
  | BrkErr
  | VoidErr
  | MainErr
  | DivZero
    deriving (Show)

showT :: Type -> String
showT (Int _) = "int"
showT (Str _) = "string"
showT (Bool _) = "bool"
showT (Void _) = "void"
showT (Fun _ _ _) = "function"

showI :: Ident -> String
showI (Ident s) = s

errorMsg :: ErrType -> String
errorMsg (TypeErr s) = "Mismatching type. Expected " ++ s
errorMsg (MulDecl s) = "Multiple declaration of " ++ s
errorMsg (Undecl s) = "Undeclared variable " ++ s
errorMsg Uninit = "Uninitialized value"
errorMsg NoRet = "Function returns no value"
errorMsg (ArgNum ident i1 i2) =
  "Invalid number of arguments for function " ++ showI ident ++
  ". Expected " ++ show i1 ++ ", got " ++ show i2
errorMsg (NotFun s) = "Not a function: " ++ s;
errorMsg NotRef  = "Not a variable name"
errorMsg DiffRets = "Mismatching types of the return statements"
errorMsg CntErr = "Continue statement outside loop"
errorMsg BrkErr = "Break statement outside loop"
errorMsg VoidErr = "Cannot print void value"
errorMsg MainErr = "Error : Wrong type of function main."
errorMsg DivZero = "Division by 0"
