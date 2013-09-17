main :: IO ()
main = getContents >>= putStr . firstNLines 10

firstNLines :: Int -> String -> String
firstNLines n = unlines . take n . lines
