import LineParser

main :: IO ()
main = do interact f
          putStrLn ""
  where
    f cs = case parse document (lines cs) of
             Right x -> x
             Left msg -> msg

document :: LineParser String
document = do many $ firstChar (== 'a')
              many1 $ firstChar (== 'b')
              eof
              return "OK"
