import System
import List

main = do args <- getArgs
          cs <- getContents
          putStr $ fgrep (head args) cs

fgrep :: String -> String -> String
fgrep pattern = unlines . filter (match pattern) . lines

match :: String -> String -> Bool
match pattern = any (pattern `isPrefixOf`) . tails
