-- Another tounix implementation.

main = do cs <- getContents
          putStr $ removeCR cs

removeCR :: String -> String
removeCR cs = filter notCR cs

notCR c = c /= '\r'
