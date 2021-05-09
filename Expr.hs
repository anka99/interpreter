module Expr where

import Turbo

import AbsEmm
import LexEmm
import ParEmm
import PrintEmm
import SkelEmm ()

import Control.Monad.Except
import Control.Monad.State
import System.IO
import Control.Monad.Reader

import Prelude

eval :: Expr -> TurboMonad Value
eval (ELitInt pos x) = return $ IntVal x

eval (EMul pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  return $ IntVal $ evalMulOp op v1 v2

eval (EAdd pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  return $ IntVal $ evalAddOp op v1 v2

eval (Neg pos e) = do
  IntVal v <- eval e
  return  $ IntVal $ (-1) * v

eval (EVar pos ident) = getLoc ident >>= getVal
  -- return $ lookup (lookup ident env) store


evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Times pos) = (*)
evalMulOp (Div pos) = div
evalMulOp (Mod pos) = mod

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp (Plus pos) = (+)
evalAddOp (Minus pos) = (-)
