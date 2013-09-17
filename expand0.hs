main = do cs <- getContents
          putStr $ expand cs

expand :: String -> String
expand cs = map translate cs

translate :: Char -> Char
translate c = if c == '\t' then '@' else c
