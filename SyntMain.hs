module Main where
import Lex
import Synt

main = do 
  inStr <- getContents
  case alexScanTokens inStr of
    Right lst -> do
      case synt lst of
        Just tree -> do
          putStrLn.show $ tree
        Nothing -> do
         putStrLn "Syntax error"
    Left err -> do
      putStrLn $ err

