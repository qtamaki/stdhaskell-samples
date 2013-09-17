import System
import List

main = do args <- getArgs
          cs <- getContents
          case args of
            [pattern] -> putStr $ fgrep pattern cs
            _         -> error "wrong argument"

fgrep :: String -> String -> String
fgrep pattern = unlines . filter (match pattern) . lines 

match :: String -> String -> Bool
match pattern = any (pattern `isPrefixOf`) . tails
