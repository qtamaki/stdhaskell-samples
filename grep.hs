import System
import Maybe
import Text.Regex

main = do args <- getArgs
          case args of
            (pat:_) -> putStr . grep (mkRegex pat) =<< getContents
            _       -> error "no pattern given"

grep :: Regex -> String -> String
grep re = unlines . filter (isJust . matchRegex re) . lines
