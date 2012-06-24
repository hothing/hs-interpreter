module Main where
import Lex
import Synt

main = do 
  inStr <- getContents
  case alexScanTokens inStr of
    Right lst -> do
      let parseTree = synt lst
      putStrLn.show $ parseTree
    Left err -> do
      putStrLn $ err

