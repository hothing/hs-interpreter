module Main where
import Lex
import Synt

main = do 
  inStr <- getContents
  case alexScanTokens inStr >>= synt of
    Right tree -> do
       putStrLn.show $ tree
    Left err -> do
      putStrLn $ "ERROR: " ++ err

