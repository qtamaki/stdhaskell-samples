import System.Environment

main = putStr . concat =<< mapM readFile =<< getArgs
