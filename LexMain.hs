module Main where
import Lex

main = do
  s <- getContents
  case alexScanTokens s of
    Right lst -> do
      putStrLn $ concatMap (\t -> "'" ++ show t ++ "' ") lst
    Left err -> do
      putStrLn $ "ERROR: " ++ err
