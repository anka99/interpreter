module Eval where

import Turbo
import Memory ( declareInit,
                increaseScope,
                declareNoInit,
                changeVal,
                readVal,
                getLoc,
                setLoc )
import Err

import AbsEmm
import LexEmm
import ParEmm
import PrintEmm
import SkelEmm ()

import Control.Monad.Except
import Control.Monad.State
import System.IO
import Control.Monad.Reader

import qualified Data.List as L


------------------------ Declarations ------------------------------------------


interpretDeclList :: [Decl] -> TurboMonad Env
interpretDeclList [] = ask
interpretDeclList (d:l) = do
  env <- interpretDecl d
  local (const env) $ interpretDeclList l

interpretDecl :: Decl -> TurboMonad Env
interpretDecl (Decl pos t items) = addItemList items
interpretDecl (FnDecl pos t i args block) = do
  env <- ask
  env' <- declareInit pos i $ FnVal t env args $ block
  local (const env') $ changeVal pos i $ FnVal t env' args $ block

addItemList :: [Item] -> TurboMonad Env
addItemList [] = ask
addItemList (i:l) = do
  env <- addItem i
  local (const env) $ addItemList l

addItem :: Item -> TurboMonad Env
addItem (NoInit pos ident) = declareNoInit pos ident
addItem (Init pos ident expr) = eval expr >>= declareInit pos ident
-- TODO array

------------------------ Statements --------------------------------------------


interpretStmt :: Stmt -> TurboMonad RetType
interpretStmt (Empty pos) = ask >>= noReturn
interpretStmt (SDecl pos decl) = interpretDecl decl >>= noReturn
interpretStmt (BStmt pos block) = do
  res <- interpretBlock block
  case res of
    RetEnv env -> ask >>= noReturn
    _ -> return $ res

interpretStmt (Ret pos e) = eval e >>= returnVal

interpretStmt (Ass pos ident expr) = do
  val <- eval expr
  changeVal pos ident val >>= noReturn

interpretStmt (Cond pos expr s) =
  executeCondAlt pos expr (interpretStmt s) $ ask >>= noReturn

interpretStmt (CondElse pos expr s1 s2) =
  executeCondAlt pos expr (interpretStmt s1) (interpretStmt s2)

interpretStmt (While pos e s) = do
  res <- executeCondAlt pos e (interpretStmt s) $ ask >>= noReturn
  case res of
    (RetBrk p) -> ask >>= noReturn
    (RetVal v) -> return res
    _ ->  executeCondAlt pos e (interpretStmt (While pos e s)) $ ask >>= noReturn

interpretStmt (Incr pos ident) = do
  val <- readVal pos ident
  case val of
    IntVal x -> changeVal pos ident (IntVal (x + 1)) >>= noReturn
    _ -> throwError $ errorPosR pos $ errorMsg $ TypeErr "int"

interpretStmt (Decr pos ident) = do
  val <- readVal pos ident
  case val of
    IntVal x -> changeVal pos ident (IntVal (x - 1)) >>= noReturn
    _ -> throwError $ errorPosR pos $ errorMsg $ TypeErr "int"

interpretStmt (SExp pos e) = interpretSExp pos e

interpretStmt (Brk pos) = return $ RetBrk pos

interpretStmt (Cnt pos) = return $ RetCnt pos

interpretStmt s = do
  liftIO $ putStrLn $ show s
  ask >>= noReturn

handleBreak :: RetType -> TurboMonad RetType -> TurboMonad RetType
handleBreak (RetBrk pos) cont = ask >>= noReturn
handleBreak (RetVal val) cont = return $ RetVal val
handleBreak _ cont = cont

interpretSExp :: Position -> Expr -> TurboMonad RetType
interpretSExp p1 (EApp p2 (Ident "print") [e]) = do
  v <- eval e
  liftIO $ putStrLn $ show v
  ask >>= noReturn

interpretSExp p1 (EApp p2 (Ident "print") l) =
  throwError $ errorPosR p1 $ errorMsg $ ArgNum (Ident "print") (toInteger $ L.length l) 1

interpretSExp p e = eval e >> ask >>= noReturn

executeCondAlt :: Position -> Expr -> TurboMonad RetType -> TurboMonad RetType -> TurboMonad RetType
executeCondAlt pos expr alt1 alt2 = do
  val <- eval expr
  case val of
    BoolVal True -> alt1
    BoolVal False -> alt2
    _ -> throwError $ show pos ++ "Non-boolean value as a result of conditional statement."

interpretBlock :: Block -> TurboMonad RetType
interpretBlock (Block p stmt) = do
  local increaseScope $ interpretStmtList stmt

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
    RetEnv env -> local (const env) $ interpretStmtList l
    r -> return r


------------------------ Expressions -------------------------------------------


eval :: Expr -> TurboMonad Value
eval (ELitInt pos x) = return $ IntVal x
eval (ELitTrue pos) = return $ BoolVal True
eval (ELitFalse pos) = return $ BoolVal False

eval (EMul pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  assertNotZero op v1 v2

eval (EAdd pos e1 op e2) = do
  v1 <- eval e1 >>= evalIntValSafe pos
  v2 <- eval e2 >>= evalIntValSafe pos
  return $ IntVal $ evalAddOp op v1 v2

eval (Neg pos e) = do
  IntVal v <- eval e
  return  $ IntVal $ (-1) * v

eval (EVar pos ident) = readVal pos ident

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
evalRel pos (BoolVal b1) (EQU p) (BoolVal b2) = return $ BoolVal $ evalRelOp (EQU p) b1 b2

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
    _ -> throwError $ errorPosR pos $ errorMsg $ TypeErr "int"

evalBoolValSafe :: Position -> Value -> TurboMonad Bool
evalBoolValSafe pos v = do
  case v of
    BoolVal b -> return b
    _ -> throwError $ errorPosR pos $ errorMsg $ TypeErr "bool"

assertNotZero :: MulOp -> Integer -> Integer -> TurboMonad Value
assertNotZero (Div p) v 0
  | v == 0 = return $ IntVal 1
  | otherwise = throwError $ errorPosR p $ errorMsg DivZero
assertNotZero op v1 v2 = return $ IntVal $ (evalMulOp op) v1 v2

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Times pos) = (*)
evalMulOp (Div pos) = div
evalMulOp (Mod pos) = mod

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp (Plus pos) = (+)
evalAddOp (Minus pos) = (-)

evalFunction :: Position -> Ident -> [Expr] -> TurboMonad Value
evalFunction pos ident exprs = do
  fun <- readVal pos ident
  case fun of
    FnVal t env args b -> do
      globEnv <- ask
      envDecl <- local (const $ increaseScope env) $ evalArgs pos ident exprs args globEnv
      local (const envDecl) $ evalFunStmt pos t b
    _ -> throwError $ errorPosR pos $ errorMsg $ NotFun $ show ident

evalFunStmt:: Position -> Type -> Block -> TurboMonad Value
evalFunStmt p (Void _) b = do
  interpretBlock b
  return $ IntVal 0

evalFunStmt p _ b = do
  ret <- interpretBlock b
  case ret of
    RetVal v -> return v
    _ -> throwError $ errorPosR p $ errorMsg NoRet

evalArgs :: Position -> Ident -> [Expr] -> [Arg] -> Env -> TurboMonad Env
evalArgs pos ident exprs args globEnv = do
  case L.length exprs == L.length args of
    True -> declareArgList exprs args globEnv
    False -> throwError $ errorPosR pos $ errorMsg $
      ArgNum ident (toInteger $ L.length args) (toInteger $ L.length exprs) 

declareArgList :: [Expr] -> [Arg] -> Env -> TurboMonad Env
declareArgList [] [] ge = ask
declareArgList (e:el) (a:al) ge = do
  case a of
    Arg pos t ident -> do
      val <- local (const ge) $ eval e
      env <- declareInit pos ident val
      local (const env) $ declareArgList el al ge
    ArgRef pos t ident -> do
      case e of
        EVar p identExp -> do
          loc <- local (const ge) $ getLoc p identExp
          env <- setLoc ident loc
          local (const env) $ declareArgList el al ge
        _ -> throwError $ errorPosR pos $ errorMsg NotRef
