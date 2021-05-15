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

import qualified Data.Map as M
import qualified Data.List as L


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

addItemVal :: Ident -> Value -> TurboMonad Env
addItemVal ident val = do
  env <- ask
  store <- get
  setVal (newLoc store) val
  return $ M.insert ident (newLoc store) env

addItem :: Item -> TurboMonad Env
addItem (NoInit pos ident) = addItemVal ident NullVal
addItem (Init pos ident expr) = eval expr >>= addItemVal ident
-- TODO array

------------------------ Statements --------------------------------------------


interpretStmt :: Stmt -> TurboMonad RetType
interpretStmt (SDecl pos decl) = interpretDecl decl >>= noReturn
interpretStmt (BStmt pos block) = interpretBlock block
interpretStmt (Ret pos e) = eval e >>= returnVal

interpretStmt (Ass pos ident expr) = do
  loc <- getLoc pos ident
  val <- eval expr
  setVal loc val >>= noReturn

interpretStmt (Cond pos expr s) =
  executeCondAlt pos expr (interpretStmt s) $ ask >>= noReturn

interpretStmt (CondElse pos expr s1 s2) =
  executeCondAlt pos expr (interpretStmt s1) (interpretStmt s2)

interpretStmt (While pos e s) = do
  executeCondAlt pos e (interpretStmt s) $ ask >>= noReturn
  executeCondAlt pos e (interpretStmt (While pos e s)) $ ask >>= noReturn

interpretStmt (Incr pos ident) = do
  val <- getIdentValue pos ident
  case val of
    IntVal x -> (setIdentValue pos ident $ IntVal (x + 1)) >>= noReturn
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "int"

interpretStmt (Decr pos ident) = do
  val <- getIdentValue pos ident
  case val of
    IntVal x -> (setIdentValue pos ident $ IntVal (x - 1)) >>= noReturn
    _ -> throwError $ errorPos pos $ errorMsg $ TypeErr "int"

interpretStmt (SExp pos e) = interpretSExp pos e

interpretStmt (Brk pos) = return $ RetBrk pos

interpretStmt (Cnt pos) = return $ RetCnt pos

interpretStmt s = do
  liftIO $ putStrLn $ show s
  ask >>= noReturn

interpretSExp :: Position -> Expr -> TurboMonad RetType
interpretSExp p1 (EApp p2 (Ident "print") [e]) = do
  v <- eval e
  liftIO $ putStrLn $ show v
  ask >>= noReturn

interpretSExp p1 (EApp p2 (Ident "print") l) =
  throwError $ errorPos p1 $ errorMsg $ ArgNum (Ident "print") (toInteger $ L.length l) 1

interpretSExpr _ _ = ask >>= noReturn --TODO: wypisywać w interactive


executeCondAlt :: Position -> Expr -> TurboMonad RetType -> TurboMonad RetType -> TurboMonad RetType
executeCondAlt pos expr alt1 alt2 = do
  val <- eval expr
  case val of
    BoolVal True -> alt1
    BoolVal False -> alt2
    _ -> throwError $ show pos ++ "Non-boolean value as a result of conditional statement."

interpretBlock :: Block -> TurboMonad RetType
interpretBlock (Block p stmt) = do
  env <- ask
  local (changeEnvTo env) $ interpretStmtList stmt

data RetType = RetVal Value | RetBrk Position | RetCnt Position | RetEnv Env

noReturn :: Env -> TurboMonad RetType
noReturn e = return $ RetEnv e

returnVal :: Value -> TurboMonad RetType
returnVal v = return $ RetVal v

interpretStmtList :: [Stmt] -> TurboMonad RetType
interpretStmtList [] = ask >>= noReturn
interpretStmtList (s:l) = do
  ret <- interpretStmt s
  case ret of
    RetEnv env -> local (changeEnvTo env) $ interpretStmtList l
    r -> return r


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

eval (EAnd pos e1 e2) = evalAndOr pos e1 e2 (&&)

eval (EOr pos e1 e2) = evalAndOr pos e1 e2 (||)

eval (Not pos e) = do
  b <- eval e >>= evalBoolValSafe pos
  return $ BoolVal $ not b

eval (ERel pos e1 op e2) = do
  v1 <- eval e1
  v2 <- eval e2
  evalRel pos v1 op v2

eval (EString pos s) = return $ StringVal s

eval (EApp pos ident exprs) = evalFunction pos ident exprs


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

evalFunction :: Position -> Ident -> [Expr] -> TurboMonad Value
evalFunction pos ident exprs = do
  fun <- getIdentValue pos ident
  case fun of
    FnVal env args s -> do
      globEnv <- ask
      envDecl <- local (changeEnvTo env) $ evalArgs pos ident exprs args globEnv
      local (changeEnvTo envDecl) $ evalFunStmt s
    _ -> throwError $ errorPos pos $ errorMsg $ NotFun $ show ident


evalFunStmt:: Stmt -> TurboMonad Value
evalFunStmt s = do
  ret <- interpretStmt s
  case ret of
    RetVal v -> return v
    _ -> throwError "Function does not return"

evalArgs :: Position -> Ident -> [Expr] -> [Arg] -> Env -> TurboMonad Env
evalArgs pos ident exprs args globEnv = do
  case L.length exprs == L.length args of
    True -> declareArgList exprs args globEnv
    False -> throwError $ errorPos pos $ errorMsg $
      ArgNum ident (toInteger $ L.length exprs) (toInteger $ L.length args)

addArg ::  Value -> Arg -> TurboMonad Env
addArg val (Arg pos t ident) = addItemVal ident val
--TODO : referencje

declareArgList :: [Expr] -> [Arg] -> Env -> TurboMonad Env
declareArgList [] [] ge = ask
declareArgList (e:el) (a:al) ge = do
  case a of
    Arg pos t ident -> do
      val <- local (changeEnvTo ge) $ eval e
      env <- addItemVal ident val
      local (changeEnvTo env) $ declareArgList el al ge
    ArgRef pos t ident -> do
      case e of
        EVar p identExp -> do
          loc <- local (changeEnvTo ge) $ getLoc p identExp
          env <- setLoc ident loc
          local (changeEnvTo env) $ declareArgList el al ge
        _ -> throwError $ errorPos pos $ errorMsg NotRef
