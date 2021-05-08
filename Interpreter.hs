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

data Value = ValInt Integer | ValBool Bool | ValString String | ValFunc Env [Arg] Block | ValNull
    deriving (Eq, Ord)

type Loc = Int
type Env = Map Ident Loc
type Store = Map Loc Ident

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
      (res, store) <- runTurbo empty empty $ interpretProgram tree
      putStrLn $ show res
      -- exitWith interpretProgram tree
  where
  parsedContent = pProgram $ myLexer content

interpretProgram :: Program -> TurboMonad Integer
interpretProgram (Program pos decl) = do
  env <- interpretDeclList decl
  return 0

newLoc :: Store -> Loc
newLoc s = size s

-- TODO capp
interpretDeclList :: [Decl] -> TurboMonad Env
interpretDeclList [] = ask
interpretDeclList (d:l) = do
  env <- interpretDecl d
  local (const env) $ interpretDeclList l

interpretDecl :: Decl -> TurboMonad Env
interpretDecl (Decl pos t []) = ask
interpretDecl (Decl pos t (i:l)) = do
  env <- declareItem i t
  local (const env) $ interpretDecl $ Decl pos t l

declareItem :: Item -> Type -> TurboMonad Env
declareItem (NoInit pos ident) t = do
  store <- get
  env <- ask
  return $ insert ident (newLoc store) env
