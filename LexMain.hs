module Main where
import Lex

main = do
  s <- getContents
  putStrLn $ concatMap (\s -> "'" ++ show s ++ "' ") $ alexScanTokens s
