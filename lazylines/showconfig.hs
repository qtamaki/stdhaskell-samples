module Main (main) where

import Config
import System.Environment

main = do args <- getArgs
          case args of
            [] -> putStrLn "no argument given"
            _  -> mapM_ (\a -> print =<< loadConfig a) args
