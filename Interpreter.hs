module Interpreter where

import Prelude
import Data.Map
import System.Exit ( exitFailure, exitSuccess, exitWith )

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm
import LexEmm
import ParEmm
import PrintEmm
import SkelEmm ()

type Err        = Either String
type ParseFun a = [Token] -> Err a

data Value = IntVal Integer | BoolVal Bool | StringVal String | FuncVal Env [Arg] Block | NullVal
    deriving (Eq, Ord, Show)

type Loc = Int
type Env = Map Ident Loc
type Store = Map Loc Value

-- TODO : być może coś zmienić, bo Mr. C.
type TurboMonad a = ReaderT Env (ExceptT String (StateT Store IO)) a

runTurbo :: Env -> Store -> TurboMonad a -> IO (Either String a, Store)
runTurbo env store e = runStateT (runExceptT (runReaderT e env)) store

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

changeEnvTo :: Env -> Env -> Env
changeEnvTo to from = to

interpretDeclList :: [Decl] -> TurboMonad Env
interpretDeclList [] = ask
interpretDeclList (d:l) = do
  env <- interpretDecl d
  local (changeEnvTo env) $ interpretDeclList l

interpretDecl :: Decl -> TurboMonad Env
interpretDecl (Decl pos t items) = addItemList items

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

intValArithm :: (Integer -> Integer -> Integer) -> Value -> Value -> TurboMonad Value
intValArithm fun (IntVal v1) (IntVal v2) = return $ IntVal $ fun v1 v2


eval :: Expr -> TurboMonad Value
eval (ELitInt pos x) = return $ IntVal x

eval (EMul pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  return $ IntVal $ v1 * v2

eval (EAdd pos e1 op e2) = do
  IntVal v1 <- eval e1
  IntVal v2 <- eval e2
  return $ IntVal $ v1 + v2

eval (Neg pos e) = do
  IntVal v <- eval e
  return  $ IntVal $ (-1) * v
