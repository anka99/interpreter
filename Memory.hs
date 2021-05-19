module Memory where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map as Map

import AbsEmm

import Turbo
import Err

-- declareInit,
--                 changeEnvTo,
--                 increaseScope,
--                 declareNoInit,
--                 changeVal,
--                 readVal,
--                 getLoc,
--                 setLoc

declareInit :: Position -> Ident -> Value -> TurboMonad Env
declareInit p i v = do
  (env, currScope) <- ask
  case Map.lookup i env of
    Just (l, sc) -> do
      case sc < currScope of
        False -> throwError $ errorPosR p $ errorMsg $ MulDecl $ show i
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
    Nothing -> throwError $ errorPosR p $ errorMsg $ Undecl $ show i

readVal :: Position -> Ident -> TurboMonad Value
readVal p i = do
  (env, scope) <- ask
  case Map.lookup i env of
    Just (l, sc) -> getVal p l
    Nothing -> throwError $ errorPosR p $ errorMsg $ Undecl $ show i

getLoc :: Position -> Ident -> TurboMonad Loc
getLoc pos ident = do
  (env, scope) <- ask
  case Map.lookup ident env of
    Just (loc, sc) -> return loc
    Nothing -> throwError $ errorPosR pos $ errorMsg $ Undecl $ show ident

getVal :: Position -> Loc -> TurboMonad Value
getVal pos loc = do
  store <- get
  case Map.lookup loc store of
    Just NullVal -> throwError $ errorPosR pos $ errorMsg Uninit
    Just val -> return val
    Nothing -> throwError $ errorPosR pos $ errorMsg NoRet

setLoc :: Ident -> Loc -> TurboMonad Env
setLoc i l = do
  (env, scope) <- ask
  return $ (Map.insert i (l, scope) env, scope)

newLoc :: Store -> Loc
newLoc s = Map.size s

increaseScope :: Env -> Env
increaseScope (env, scope) = (env, scope + 1)
