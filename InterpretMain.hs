module Main where
import Lex
import Synt
import Interpret

evalLoop ctx (x:xs) = do
  case alexScanTokens x of
    Right lst -> do
      let parseTree = synt lst
      case evalProgCtx ctx parseTree of
        Right newCtx -> do
          putStrLn $ show newCtx
          evalLoop newCtx xs
        Left err -> do
          putStrLn $ "ERROR: " ++ err
          evalLoop ctx xs
    Left err -> do
      putStrLn $ "ERROR: " ++ err
      evalLoop ctx xs

evalLoop _ [] = return ()

main = do
  input <- getContents
  evalLoop createContext $ lines input
