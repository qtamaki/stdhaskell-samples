import IO
import System
import Text.ParserCombinators.Parsec

main = do args <- getArgs
          case args of
            [pat] -> do putStr "expand  : "; print $ expand pat
                        putStr "expand2 : "; print $ expandPattern pat
                        putStr "expand2 : "; print $ expandPattern2 pat
            _     -> usage

usage = do p <- getProgName
           die ("Usage: " ++ p ++ " <pattern>")

die msg = do hPutStrLn stderr msg
             exitWith (ExitFailure 1)

expand :: String -> [String]
expand pattern = case parse estring "" pattern of
                   Right x -> expandConcat x
                   Left err -> ["parse error (expand)"]

expandPattern :: String -> [String]
expandPattern pattern = expandCharClass pattern >>= expandAltWords

expandPattern2 :: String -> [String]
expandPattern2 pattern =
    return pattern >>= expandCharClass >>= expandAltWords

expandCharClass :: String -> [String]
expandCharClass pattern = case parse cstring "" pattern of
                            Right x -> expandConcat x
                            Left err -> ["parse error (char class)"]

expandAltWords :: String -> [String]
expandAltWords pattern = case parse astring "" pattern of
                           Right x -> expandConcat x
                           Left err -> [show err]

expandConcat :: [[String]] -> [String]
expandConcat [] = []
expandConcat [ws] = ws
expandConcat (ws:ss) = do w <- ws
                          s <- expandConcat ss
                          return (w ++ s)

estring = many ecomponent

ecomponent =  do w <- many1 (noneOf "[{")
                 return [w]
          <|> charclass
          <|> altwords

cstring = many ccomponent

ccomponent =  do w <- many1 (noneOf "[")
                 return [w]
          <|> charclass

charclass = do char '['
               cs <- many1 (noneOf "]")
               char ']'
               return $ map (\c -> [c]) cs

astring = do ws <- many acomponent
             eof
             return ws

acomponent =  do w <- many1 (noneOf "{")
                 return [w]
          <|> altwords

altwords = do char '{'
              ws <- content
              char '}'
              return ws
  where
    content = chainr1 word comma

    word = do w <- many (noneOf ",}")
              return [w]

    comma = do char ','
               return (++)
