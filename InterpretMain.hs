module Main where
import Lex
import Synt
import Interpret
main = do
--  input <- getContents
  let parseTree = synt.alexScanTokens $ "a = 1; b = 2 + d; c = a + b;"
  let out = case evalProg parseTree of
            Right x -> show x
            Left y -> "ERROR: " ++ y
  putStrLn out 
