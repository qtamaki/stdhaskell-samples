import System

main = putStr . concat =<< mapM readFile =<< getArgs
