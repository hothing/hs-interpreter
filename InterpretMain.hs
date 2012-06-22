module Main where
import Lex
import Synt
import Interpret

evalLoop ctx (x:xs) = do
  let parseTree = synt.alexScanTokens $ x
  case evalProgCtx ctx parseTree of
    Right newCtx -> do
      putStrLn $ show newCtx
      evalLoop newCtx xs
    Left err -> do
      putStrLn $ "ERROR: " ++ err
      evalLoop ctx xs

evalLoop _ [] = do return ()

main = do
  input <- getContents
  evalLoop createContext $ lines input
