module Main where
import Lex
import Synt

main = do 
  inStr <- getContents
  let parseTree = synt.alexScanTokens $ inStr  
  putStrLn.show $ parseTree

