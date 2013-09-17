import Char

main = do cs <- getContents
          print $ parseConfig cs

parseConfig :: String -> [(String, String)]
parseConfig = map parseLine . filter (not . isBlank) . map removeComment . lines

parseLine :: String -> (String, String)
parseLine line = let (key, ('=':value)) = break (== '=') line
                 in (strip key, strip value)

removeComment :: String -> String
removeComment = fst . break (== '#')

isBlank :: String -> Bool
isBlank = null . strip

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
