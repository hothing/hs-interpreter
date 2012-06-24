module Main where
import Lex
import Synt
import Interpret

evalLoop ctx (x:xs) = do
  case alexScanTokens x of
    Right lst -> do
      case synt lst of
        Just tree -> case evalProgCtx ctx tree of
          Right newCtx -> do
            putStrLn $ show newCtx
            evalLoop newCtx xs
          Left err -> do
            putStrLn $ "ERROR: " ++ err
            evalLoop ctx xs
        Nothing -> do
          putStrLn "ERROR: syntax error"
          evalLoop ctx xs
    Left err -> do
      putStrLn $ "ERROR: " ++ err
      evalLoop ctx xs

evalLoop _ [] = return ()

main = do
  input <- getContents
  evalLoop createContext $ lines input
