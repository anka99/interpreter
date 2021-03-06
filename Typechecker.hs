module Typechecker where

import Control.Monad.Except
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm

import Err

import qualified Data.Map as Map
import qualified Data.List as L
import System.Exit ( exitFailure )

type Position = BNFC'Position
type Scope = Int
type Env = (Map.Map Ident (Type, Position, Scope), Scope)

type Typechecker a = ReaderT Env (ExceptT String IO) a

runTypechecker :: Env -> Typechecker a -> IO (Either String a)
runTypechecker env e = runExceptT (runReaderT e env)

emptyEnv :: Env
emptyEnv = (Map.empty, 0)

checkTypes :: Program -> Typechecker Env
checkTypes (Program pos decls) = do
  env <- checkDeclList decls
  local (const env) $ checkMain

checkMain :: Typechecker Env
checkMain = do
  main <- getValSafe (Just (1, 1)) (Ident "main")
  case main of
    (Fun _ (Int _) [], p, s) -> ask
    (_, p, _) -> throwError $ errorPos p $ errorMsg MainErr

checkProgramTypes :: Program -> IO ()
checkProgramTypes tree = do
  check <- runTypechecker emptyEnv (checkTypes tree)
  case check of
    Left err -> do
      putStrLn err
      exitFailure
    Right _ -> return ()

addVal :: Ident -> Type -> Position -> Typechecker Env
addVal i tp pos = do
  (env, currScope) <- ask
  case Map.lookup i env of
    Nothing -> return $ (Map.insert i (tp, pos, currScope) env, currScope)
    Just (t, p, sc) -> do
      case sc < currScope of
        False -> throwError $ errorPos p $ errorMsg $ MulDecl $ showI i
        True -> return $ (Map.insert i (tp, pos, currScope) env, currScope)

getVal :: Ident -> Typechecker (Maybe (Type, Position, Scope))
getVal i = do
  (env, scope) <- ask
  return $ Map.lookup i env

getValSafe :: Position -> Ident -> Typechecker (Type, Position, Scope)
getValSafe pos ident = do
  val <- getVal ident
  case val of
    Nothing -> throwError $ errorPos pos $ errorMsg $ Undecl $ showI ident
    Just (t, p, s) -> return (t, p, s)

checkDeclList :: [Decl] -> Typechecker Env
checkDeclList [] = ask
checkDeclList (d:l) = do
  env <- checkDecl d
  local (const env) $ checkDeclList l

checkDecl :: Decl -> Typechecker Env
checkDecl (Decl p t itl) = addItemList t itl
checkDecl (FnDecl pos funT ident args block) = do
  env <- addVal ident (Fun pos funT $ getArgListTypes args) pos
  env' <- local (const $ increaseScope env) $ addArgList args
  res <- local (const env') $ checkFunBlock block
  case res of
    RetVal p t -> do
      case compareTypes t funT of
        True -> return env
        False -> throwError $ errorPos p $ errorMsg $ TypeErr $ showT funT
    RetEnv _ -> do
      case funT of
        Void _ -> return env
        _ -> throwError $ errorPos pos $ errorMsg $ NoRet
    RetCnt p -> throwError $ errorPos p $ errorMsg $ CntErr
    RetBrk p -> throwError $ errorPos p $ errorMsg $ BrkErr

addArgList :: [Arg] -> Typechecker Env
addArgList [] = ask
addArgList (a:l) = do
  env <- addArg a
  local (const env) $ addArgList l

addArg :: Arg -> Typechecker Env
addArg (Arg p t i) = addVal i t p
addArg (ArgRef p t i) = addVal i t p

checkFunBlock :: Block -> Typechecker RetType
checkFunBlock (Block p stmts) = checkStmtList stmts

getArgListTypes:: [Arg] -> [Type]
getArgListTypes = L.map getArgType

getArgType :: Arg -> Type
getArgType (Arg p t i) = t
getArgType (ArgRef p t i) = t

addItemList :: Type -> [Item] -> Typechecker Env
addItemList t [] = ask
addItemList t (i:l) = do
  env <- addItem t i
  local (const env) $ addItemList t l

addItem :: Type -> Item -> Typechecker Env
addItem t (NoInit pos ident) = addVal ident t pos
addItem t (Init pos ident expr) = do
  env <- addVal ident t pos
  local (const env) $ checkAss pos ident expr
  return env

checkAss :: Position -> Ident -> Expr -> Typechecker ()
checkAss pos ident expr = do
  (tId, p, s) <- getValSafe pos ident
  tExp <- evalExprType expr
  case compareTypes tExp tId of
    True -> return ()
    False -> throwError $ errorPos pos $ errorMsg $ TypeErr $ showT tId


------------------------ Statements --------------------------------------------


checkStmt :: Stmt -> Typechecker RetType
checkStmt (Empty pos) = ask >>= noReturn
checkStmt (SDecl pos decl) = checkDecl decl >>= noReturn
checkStmt (BStmt pos block) = checkBlock block --TODO
checkStmt (Ret pos e) = do
  t <- evalExprType e
  return $ RetVal pos t
checkStmt (Ass pos ident expr) = do
  checkAss pos ident expr
  ask >>= noReturn

checkStmt (Cond pos expr s) = do
  evalExprType expr >>= assertBool pos
  checkStmt s

checkStmt (CondElse pos expr s1 s2) = do
  evalExprType expr >>= assertBool pos
  r1 <- checkStmt s1
  r2 <- checkStmt s2
  unifyCondElseRet r1 r2

checkStmt (While pos expr s) = do
  evalExprType expr >>= assertBool pos
  res <- checkStmt s
  case res of
    RetVal p t -> return $ res
    _ -> ask >>= noReturn

checkStmt (Incr pos ident) = do
  (t, p, s) <- getValSafe pos ident
  assertInt pos t
  ask >>= noReturn

checkStmt (Decr pos ident) = checkStmt (Incr pos ident)

checkStmt (SExp p e) = checkSExp e

checkStmt (Brk pos) = return $ RetBrk pos

checkStmt (Cnt pos) = return $ RetCnt pos

data RetType
  = RetVal Position Type
  | RetBrk Position
  | RetCnt Position
  | RetEnv Env

checkSExp :: Expr -> Typechecker RetType
checkSExp (EApp p (Ident "print") [e]) = do
  evalExprType e >>= assertNotVoid p
  ask >>= noReturn

checkSExp (EApp p (Ident "print") l) =
  throwError $ errorPos p $ errorMsg $ ArgNum (Ident "print") 1 $ toInteger $ L.length l

checkSExp e = do
  evalExprType e
  ask >>= noReturn

unifyCondElseRet :: RetType -> RetType -> Typechecker RetType
unifyCondElseRet (RetVal p1 t1) (RetVal p2 t2)
  | compareTypes t1 t2 = return $ RetVal p1 t1
  | otherwise = throwError $ errorPos p1 $ errorPos p2 $ errorMsg $ DiffRets
unifyCondElseRet (RetBrk p) _ = return $ RetBrk p
unifyCondElseRet _ (RetBrk p) = return $ RetBrk p
unifyCondElseRet (RetCnt p) _ = return $ RetCnt p
unifyCondElseRet _ (RetCnt p) = return $ RetCnt p
unifyCondElseRet _ _ = ask >>= noReturn

compareTypes :: Type -> Type -> Bool
compareTypes (Int _) (Int _) = True
compareTypes (Bool _) (Bool _) = True
compareTypes (Str _) (Str _) = True
compareTypes (Void _) (Void _) = True
compareTypes _ _ = False --TODO fun

noReturn :: Env -> Typechecker RetType
noReturn e = return $ RetEnv e

increaseScope :: Env -> Env
increaseScope (env, scope) = (env, scope + 1)

checkBlock :: Block -> Typechecker RetType
checkBlock (Block p stmts) = do
  res <- local increaseScope $ checkStmtList stmts
  case res of
    RetEnv env -> ask >>= noReturn
    _ -> return $ res

checkStmtList :: [Stmt] -> Typechecker RetType
checkStmtList [] = ask >>= noReturn
checkStmtList (s:l) = do
  ret <- checkStmt s
  case ret of
    RetEnv e -> local (const e) $ checkStmtList l
    _ -> checkStmtList l >> return ret

------------------------ Expressions -------------------------------------------


evalExprType :: Expr -> Typechecker Type
evalExprType (EVar pos ident) = do
  (t, p, s) <- getValSafe pos ident
  return t

evalExprType (ELitInt p i) = return $ Int p
evalExprType (ELitTrue p) = return $ Bool p
evalExprType (ELitFalse p) = return $ Bool p

evalExprType (EMul p e1 op e2) = do
  evalExprType e1 >>= assertInt p
  evalExprType e2 >>= assertInt p
  return $ Int p

evalExprType (EAdd p e1 op e2) = do
  evalExprType e1 >>= assertInt p
  evalExprType e2 >>= assertInt p
  return $ Int p

evalExprType (EString p s) = return $ Str p
evalExprType (Neg p e) = do
  evalExprType e >>= assertInt p
  return $ Int p

evalExprType (Not p e) = do
  evalExprType e >>= assertBool p
  return $ Bool p

evalExprType (ERel p e1 op e2) = do
  t1 <- evalExprType e1
  t2 <- evalExprType e2
  assertRel p t1 op t2
  return $ Bool p

evalExprType (EAnd p e1 e2) = do
  evalExprType e1 >>= assertInt p
  evalExprType e2 >>= assertInt p
  return $ Bool p

evalExprType (EOr p e1 e2) = evalExprType (EAnd p e1 e2)

evalExprType (EApp pos i exprs) = do
  (Fun fp ft types, p, _) <- getValSafe pos i
  case L.length exprs == L.length types of
    False -> throwError $ errorPos pos $ errorMsg $
      ArgNum i (toInteger $ L.length types) (toInteger $ L.length exprs)
    True -> assertTypeList pos types exprs
  return ft

assertTypeList :: Position -> [Type] -> [Expr] -> Typechecker ()
assertTypeList p [] [] = return ()
assertTypeList p (t:tl) (e:el) = do
  tExp <- evalExprType e
  case compareTypes t tExp of
    True -> return ()
    False -> throwError $ errorPos p $ errorMsg $ TypeErr $ showT t

assertRel:: Position -> Type -> RelOp -> Type -> Typechecker ()
assertRel p (Int p1) op (Int p2) = return ()
assertRel p (Str p1) op (Str p2) = return ()
assertRel p (Bool p1) (EQU p0) (Bool p2) = return ()
assertRel p _ _ _ = throwError $ errorPos p $ errorMsg $ TypeErr "left: string, right: string or left: int, right: int"

assertNotVoid :: Position -> Type -> Typechecker ()
assertNotVoid pos (Void p) = throwError $ errorPos pos $ errorMsg VoidErr
assertNotVoid pos _ = return ()

assertBool :: Position -> Type -> Typechecker ()
assertBool pos (Bool p) = return ()
assertBool pos _ = throwError $ errorPos pos $ errorMsg $ TypeErr "bool"

assertInt :: Position -> Type -> Typechecker ()
assertInt pos (Int p) = return ()
assertInt pos _ = throwError $ errorPos pos $ errorMsg $ TypeErr "int"
