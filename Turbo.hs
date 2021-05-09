module Turbo where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import System.IO
import Control.Monad.Reader

import AbsEmm

import qualified Data.Map as Map

data Value = IntVal Integer | BoolVal Bool | StringVal String | FuncVal Env [Arg] Block | NullVal
    deriving (Eq, Ord, Show)

type Loc = Int
type Env = Map.Map Ident Loc
type Store = Map.Map Loc Value

-- TODO : być może coś zmienić, bo Mr. C.
type TurboMonad a = ReaderT Env (ExceptT String (StateT Store IO)) a

runTurbo :: Env -> Store -> TurboMonad a -> IO (Either String a, Store)
runTurbo env store e = runStateT (runExceptT (runReaderT e env)) store

changeEnvTo :: Env -> Env -> Env
changeEnvTo to from = to
