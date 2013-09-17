import System
import Maybe
import Text.Regex

main = do args <- getArgs
          cs <- getContents
          putStr $ grep (mkRegex $ head args) cs

grep :: Regex -> String -> String
grep re cs = unlines $ filter match $ lines cs
  where
    match line = isJust (matchRegex re line)
