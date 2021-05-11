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
  env <- putIdent pos i
  store <- get
  local (changeEnvTo env) $ setVal (newLoc store) $ FnVal env args $ BStmt pos block

addItemList :: [Item] -> TurboMonad Env
addItemList [] = ask
addItemList (i:l) = do
  env <- addItem i
  local (changeEnvTo env) $ addItemList l

setValue :: Loc -> Value -> TurboMonad Env
setValue loc val = do
  store <- get
  put $ insert loc val store
  ask

addItemVal :: Ident -> Value -> TurboMonad Env
addItemVal ident val = do
  env <- ask
  store <- get
  setValue (newLoc store) val
  return $ insert ident (newLoc store) env

addItem :: Item -> TurboMonad Env
addItem (NoInit pos ident) = addItemVal ident NullVal
addItem (Init pos ident expr) = eval expr >>= addItemVal ident
-- TODO array


------------------------ Statements --------------------------------------------


interpretStmt :: Stmt -> TurboMonad Env
interpretStmt (SDecl pos decl) = interpretDecl decl
interpretStmt (BStmt pos block) = interpretBlock block
interpretStmt (Ret pos e) = eval e >>= setRetVal --TODO kończyć wykonanie funkcji

interpretStmt (Ass pos ident expr) = do
  loc <- getLoc pos ident
  val <- eval expr
  setValue loc val

interpretStmt (Cond pos expr s) =
  executeCondAlt pos expr (interpretStmt s) ask

interpretStmt (CondElse pos expr s1 s2) =
  executeCondAlt pos expr (interpretStmt s1) (interpretStmt s2)

interpretStmt (While pos e s) = do
  env <- executeCondAlt pos e (interpretStmt s) ask
  local (changeEnvTo env) $ executeCondAlt pos e (interpretStmt s) ask

interpretStmt (Incr pos ident) = do
  val <- getIdentValue pos ident
  case val of
    IntVal x -> setIdentValue pos ident $ IntVal (x + 1)
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "int"

interpretStmt (Decr pos ident) = do
  val <- getIdentValue pos ident
  case val of
    IntVal x -> setIdentValue pos ident $ IntVal (x - 1)
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "int"

-- interpretStmt ()

executeCondAlt :: Position -> Expr -> TurboMonad Env -> TurboMonad Env -> TurboMonad Env
executeCondAlt pos expr alt1 alt2 = do
  val <- eval expr
  case val of
    BoolVal True -> alt1
    BoolVal False -> alt2
    _ -> throwError $ show pos ++ "Non-boolean value as a result of conditional statement."

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
eval (ELitTrue pos) = return $ BoolVal True
eval (ELitFalse pos) = return $ BoolVal False

eval (EMul pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  return $ IntVal $ evalMulOp op v1 v2

eval (EAdd pos e1 op e2) = do
  v1 <- eval e1 >>= evalIntValSafe pos
  v2 <- eval e2 >>= evalIntValSafe pos
  return $ IntVal $ evalAddOp op v1 v2

eval (Neg pos e) = do
  IntVal v <- eval e
  return  $ IntVal $ (-1) * v

eval (EVar pos ident) = getLoc pos ident >>= (getVal pos)

--TODO : arguments
eval (EApp pos ident exprs) = evalFunction pos ident

eval (EAnd pos e1 e2) = evalAndOr pos e1 e2 (&&)

eval (EOr pos e1 e2) = evalAndOr pos e1 e2 (||)

eval (ERel pos e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalRel pos v1 op v2

evalAndOr :: Position -> Expr -> Expr -> (Bool -> Bool -> Bool) -> TurboMonad Value
evalAndOr pos e1 e2 op = do
  v1 <- eval e1 >>= evalBoolValSafe pos
  v2 <- eval e2 >>= evalBoolValSafe pos
  return $ BoolVal $ op v1 v2

evalRel :: Position -> Value -> RelOp -> Value -> TurboMonad Value
evalRel pos (IntVal v1) op (IntVal v2) = return $ BoolVal $ evalRelOp op v1 v2
evalRel pos (StringVal s1) op (StringVal s2) = return $ BoolVal $ evalRelOp op s1 s2
evalRel pos _ _ _ = throwError $ errorPos pos $ errorMsg $ TypeErr "left: string, right: string or left: int, right: int"

evalRelOp :: Ord a => RelOp -> a -> a -> Bool
evalRelOp (LTH pos) = (<)
evalRelOp (LE pos) = (<=)
evalRelOp (GTH pos) = (>)
evalRelOp (GE pos) = (>=)
evalRelOp (EQU pos) = (==)
evalRelOp (NE pos) = (/=)

evalIntValSafe :: Position -> Value -> TurboMonad Integer
evalIntValSafe pos v = do
  case v of
    IntVal x -> return x
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "int"

evalBoolValSafe :: Position -> Value -> TurboMonad Bool
evalBoolValSafe pos v = do
  case v of
    BoolVal b -> return b
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "bool"

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Times pos) = (*)
evalMulOp (Div pos) = div
evalMulOp (Mod pos) = mod

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp (Plus pos) = (+)
evalAddOp (Minus pos) = (-)

evalFunction :: Position -> Ident -> TurboMonad Value
evalFunction pos ident = do
   FnVal env args block <- getLoc pos ident >>= getVal pos
   local (changeEnvTo env) $ interpretStmt block
   getRetVal pos
