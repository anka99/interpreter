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
  | FnVal Env [Arg] Block
  | NullVal
    deriving (Eq, Ord, Show)

type Scope = Int
type Loc = Int
type Env = (Map.Map Ident (Loc, Scope), Scope)
type Store = Map.Map Loc Value

-- TODO : być może coś zmienić, bo Mr. C.env
type TurboMonad a = ReaderT Env (ExceptT String (StateT Store IO)) a

type Position = BNFC'Position

runTurbo :: Env -> Store -> TurboMonad a -> IO (Either String a, Store)
runTurbo env store e = runStateT (runExceptT (runReaderT e env)) store

emptyEnv :: Env
emptyEnv = (Map.empty, 0)
