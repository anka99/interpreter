import Prelude
  ( ($)
  , Either(..)
  , Int, (>)
  , String, (++), unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

import Interpreter

main :: IO ()
main = getArgs >>= parse

parse :: [String] -> IO ()
parse ["--help"] = usage
parse []         = getContents >>= interpret
parse fs         = mapM_ runFile fs

runFile :: FilePath -> IO ()
runFile f = readFile f >>= interpret

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Interprets stdin."
    , "  (files)         Interprets content of files."
    ]
  exitFailure
