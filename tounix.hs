main = do cs <- getContents
          putStr $ crlf2lf cs

crlf2lf :: String -> String
crlf2lf ('\r' : '\n' : cs) = '\n' : crlf2lf cs
crlf2lf (c:cs) = c : crlf2lf cs
crlf2lf "" = ""
