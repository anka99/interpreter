module Eval where

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

import Data.Map


------------------------ Declarations ------------------------------------------


interpretDeclList :: [Decl] -> TurboMonad Env
interpretDeclList [] = ask
interpretDeclList (d:l) = do
  env <- interpretDecl d
  local (changeEnvTo env) $ interpretDeclList l

interpretDecl :: Decl -> TurboMonad Env
interpretDecl (Decl pos t items) = addItemList items
interpretDecl (FnDecl pos t i args block) = do
  env <- ask
  addItemVal i $ FnVal env args block

addItemList :: [Item] -> TurboMonad Env
addItemList [] = ask
addItemList (i:l) = do
  env <- addItem i
  local (changeEnvTo env) $ addItemList l

newLoc :: Store -> Loc
newLoc s = size s

addItemVal :: Ident -> Value -> TurboMonad Env
addItemVal ident val = do
  env <- ask
  store <- get
  put $ insert (newLoc store) val store
  return $ insert ident (newLoc store) env

addItem :: Item -> TurboMonad Env
addItem (NoInit pos ident) = addItemVal ident NullVal
addItem (Init pos ident expr) = eval expr >>= addItemVal ident
-- TODO array


------------------------ Statements --------------------------------------------


interpretStmt :: Stmt -> TurboMonad Env
interpretStmt (SDecl pos decl) = interpretDecl decl
interpretStmt (BStmt pos block) = interpretBlock block

interpretBlock :: Block -> TurboMonad Env
interpretBlock (Block t stmt) = interpretStmtList stmt

interpretStmtList :: [Stmt] -> TurboMonad Env
interpretStmtList [] = ask
interpretStmtList (s:l) = do
  env <- interpretStmt s
  local (changeEnvTo env) $ interpretStmtList l
  

------------------------ Expressions -------------------------------------------


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

--TODO : arguments
eval (EApp pos ident exprs) = evalFunction ident

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Times pos) = (*)
evalMulOp (Div pos) = div
evalMulOp (Mod pos) = mod

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp (Plus pos) = (+)
evalAddOp (Minus pos) = (-)

evalFunction :: Ident -> TurboMonad Value
evalFunction ident = do
   FnVal env args block <- getLoc ident >>= getVal
   return $ IntVal 0
