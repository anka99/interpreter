module Turbo where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm

import qualified Data.Map as Map

data Value = IntVal Integer | BoolVal Bool | StringVal String | FnVal Env [Arg] Stmt | NullVal
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
    Nothing -> throwError $ errorMsg pos "Undeclared variable"

-- TODO: remove "fucking"
getVal :: Position -> Loc -> TurboMonad Value
getVal pos loc = do
  store <- get
  case Map.lookup loc store of
    Just NullVal -> throwError $ errorMsg pos "Uninitialized value"
    Just val -> return val
    Nothing -> throwError $ errorMsg pos "This should have never happend and if did we can safely assume, that author of this code is fucking stupid."

setRetVal :: Value -> TurboMonad Env
setRetVal val = do
  store <- get
  put $ Map.insert retLoc val store
  ask

getRetVal :: Position -> TurboMonad Value
getRetVal pos = getVal pos retLoc

getIdentValue :: Position -> Ident -> TurboMonad Value
getIdentValue pos ident = (getLoc pos ident) >>= (getVal pos)

errorMsg :: Position -> String -> String
errorMsg (Just (line, col)) msg =
  "Error in line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg

errorMsg Nothing _ = "Function main not found."
