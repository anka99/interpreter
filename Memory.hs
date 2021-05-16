module Memory where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map

import AbsEmm

import Turbo
import Err

retLoc :: Loc
retLoc = -1

changeEnvTo :: Env -> Env -> Env
changeEnvTo to from = to

declareInit :: Position -> Ident -> Value -> TurboMonad Env
declareInit p i v = do
  (env, currScope) <- ask
  case Map.lookup i env of
    Just (l, sc) -> do
      case sc < currScope of
        False -> throwError $ errorPos p $ errorMsg $ MulDecl $ show i
        True -> do
          store <- get
          put $ Map.insert (newLoc store) v store
          return $ (Map.insert i (newLoc store, currScope) env, currScope)
    Nothing -> do
      store <- get
      put $ Map.insert (newLoc store) v store
      return $ (Map.insert i (newLoc store, currScope) env, currScope)

declareNoInit :: Position -> Ident -> TurboMonad Env
declareNoInit p i = declareInit p i NullVal

changeVal :: Position -> Ident -> Value -> TurboMonad Env
changeVal p i v = do
  (env, scope) <- ask
  case Map.lookup i env of
    Just (l, sc) -> do
      store <- get
      put $ Map.insert l v store
      ask
    Nothing -> throwError $ errorPos p $ errorMsg $ Undecl $ show i

readVal :: Position -> Ident -> TurboMonad Value
readVal p i = do
  (env, scope) <- ask
  case Map.lookup i env of
    Just (l, sc) -> getVal p l
    Nothing -> throwError $ errorPos p $ errorMsg $ Undecl $ show i

getLoc :: Position -> Ident -> TurboMonad Loc
getLoc pos ident = do
  (env, scope) <- ask
  case Map.lookup ident env of
    Just (loc, sc) -> return loc
    Nothing -> throwError $ errorPos pos $ errorMsg $ Undecl $ show ident

getVal :: Position -> Loc -> TurboMonad Value
getVal pos loc = do
  store <- get
  case Map.lookup loc store of
    Just NullVal -> throwError $ errorPos pos $ errorMsg Uninit
    Just val -> return val
    Nothing -> throwError $ errorPos pos $ errorMsg NoRet

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

setLoc :: Ident -> Loc -> TurboMonad Env
setLoc i l = do
  (env, scope) <- ask
  return $ (Map.insert i (l, scope) env, scope)

newLoc :: Store -> Loc
newLoc s = Map.size s

putIdent :: Position -> Ident -> TurboMonad Env
putIdent pos ident = do
  (env, scope) <- ask
  case Map.lookup ident env of
    Just (l, sc) -> do
      case sc < scope of
        False -> throwError $ errorPos pos $ errorMsg $ MulDecl $ show ident
  store <- get
  return $ (Map.insert ident (newLoc store, scope) env, scope)

putIdentVal :: Position -> Ident -> Value -> TurboMonad Env
putIdentVal p i v = do
  putIdent p i
  loc <- getLoc p i
  setVal loc v

increaseScope :: Env -> Env
increaseScope (env, scope) = (env, scope + 1)
