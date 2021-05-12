module Turbo where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm

import qualified Data.Map as Map

data Value
  = IntVal Integer
  | BoolVal Bool
  | StringVal String
  | FnVal Env [Arg] Stmt
  | NullVal
    deriving (Eq, Ord, Show)

type Loc = Int
type Env = Map.Map Ident Loc
type Store = Map.Map Loc Value

-- TODO : być może coś zmienić, bo Mr. C.env
type TurboMonad a = ReaderT Env (ExceptT String (StateT Store IO)) a

type Position = BNFC'Position

retLoc :: Loc
retLoc = -1

runTurbo :: Env -> Store -> TurboMonad a -> IO (Either String a, Store)
runTurbo env store e = runStateT (runExceptT (runReaderT e env)) store

changeEnvTo :: Env -> Env -> Env
changeEnvTo to from = to

getLoc :: Position -> Ident -> TurboMonad Loc
getLoc pos ident = do
  env <- ask
  case Map.lookup ident env of
    Just loc -> return loc
    Nothing -> throwError $ errorPos pos $ errorMsg $ Undecl $ show ident

-- TODO: remove "fucking"
getVal :: Position -> Loc -> TurboMonad Value
getVal pos loc = do
  store <- get
  case Map.lookup loc store of
    Just NullVal -> throwError $ errorPos pos $ errorMsg Uninit
    Just val -> return val
    Nothing -> throwError $ errorPos pos $ errorMsg NoRet

setRetVal :: Value -> TurboMonad Env
setRetVal val = setVal retLoc val

getRetVal :: Position -> TurboMonad Value
getRetVal pos = getVal pos retLoc

getIdentValue :: Position -> Ident -> TurboMonad Value
getIdentValue pos ident = (getLoc pos ident) >>= (getVal pos)

setIdentValue :: Position -> Ident -> Value -> TurboMonad Env
setIdentValue pos ident val = do
  loc <- getLoc pos ident -- TODO maybe error
  setVal loc val

setVal :: Loc -> Value -> TurboMonad Env
setVal loc val = do
  store <- get
  put $ Map.insert loc val store
  ask

newLoc :: Store -> Loc
newLoc s = Map.size s

putIdent :: Position -> Ident -> TurboMonad Env
putIdent pos ident = do
  env <- ask
  case Map.lookup ident env of
    Just l -> throwError $ errorPos pos $ errorMsg $ MulDecl $ show ident
    Nothing -> do
      store <- get
      return $ Map.insert ident (newLoc store) env

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
  | ArgNum String Integer Integer
    deriving (Show)

errorMsg :: ErrType -> String
errorMsg (TypeErr s) = "Mismatching type. Expected " ++ s
errorMsg (MulDecl s) = "Multiple declaration of " ++ s
errorMsg (Undecl s) = "Undeclared variable " ++ s
errorMsg (Uninit) = "Uninitialized value"
errorMsg (NoRet) = "Function returns no value" --TODO remove "fucking"
errorMsg (ArgNum s i1 i2) =
  "Invalid number of arguments for function " ++ s ++ ": " ++ show i1 ++
  ".Expected " ++ show i2
