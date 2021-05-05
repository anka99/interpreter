import System.Environment ( getArgs )
import Text.Show

main :: IO ()
main = getArgs >>= parse

parse []     = putStrLn "interactive"
parse args   = putStrLn $ foldr (\arg acc -> acc ++ " " ++ arg) "" args
