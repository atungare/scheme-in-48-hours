module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Please enter your name: "
  args <- getArgs
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ", this is " ++ (args !! 0) ++ "!")
  putStrLn ("The sum of " ++ (args !! 1) ++ " and " ++ (args !! 2) ++ " is " ++ show (((read (args !! 1)) + (read (args !! 2)))))
