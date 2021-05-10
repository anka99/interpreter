module Turbo where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm

import qualified Data.Map as Map

data Value = IntVal Integer | BoolVal Bool | StringVal String | FnVal Env [Arg] Block | NullVal
    deriving (Eq, Ord, Show)

type Loc = Int
type Env = Map.Map Ident Loc
type Store = Map.Map Loc Value

-- TODO : być może coś zmienić, bo Mr. C.env
type TurboMonad a = ReaderT Env (ExceptT String (StateT Store IO)) a

runTurbo :: Env -> Store -> TurboMonad a -> IO (Either String a, Store)
runTurbo env store e = runStateT (runExceptT (runReaderT e env)) store

changeEnvTo :: Env -> Env -> Env
changeEnvTo to from = to

getLoc :: Ident -> TurboMonad Loc
getLoc ident = do
  env <- ask
  case Map.lookup ident env of
    Just loc -> return loc
    Nothing -> throwError "Undeclared variable"

-- TODO: remove "fucking"
getVal :: Loc -> TurboMonad Value
getVal loc = do
  store <- get
  case Map.lookup loc store of
    Just NullVal -> throwError "Uninitialized value"
    Just val -> return val
    Nothing -> throwError "This should have never happend and if did we can safely assume, that author of this code is fucking stupid."

-- getValue :: Loc -> TurboMonad (Maybe Value)
-- getValue loc = return $ get >>= lookup loc

-- getVal :: Ident -> TurboMonad (Maybe Value)
-- getVal ident = do
--   store <- get
--   env <- ask
--   return $ lookup (lookup )
