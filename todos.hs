main = do cs <- getContents
          putStr $ concatMap lf2crlf cs

lf2crlf :: Char -> String
lf2crlf '\n' = "\r\n"
lf2crlf c = [c]
