main = do cs <- getContents
          putStr $ tac cs

tac :: String -> String
tac = unlines . reverse . lines
