main = do cs <- getContents
          putStr $ map swapa cs

swapa :: Char -> Char
swapa 'a' = 'A'
swapa 'A' = 'a'
swapa c = c
