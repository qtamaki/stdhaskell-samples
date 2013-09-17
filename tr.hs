import List
import Maybe
import System

main :: IO ()
main = do args <- getArgs
          case args of
            [src, dest] -> do cs <- getContents
                              putStr $ tr (zip src dest) cs
            _           -> error "Usage: tr SRC DEST < file"

tr :: [(Char, Char)] -> String -> String
tr charmap cs = map translate cs
  where
    translate :: Char -> Char
    translate c = fromMaybe c $ lookup c charmap
