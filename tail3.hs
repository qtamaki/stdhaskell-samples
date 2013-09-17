main :: IO ()
main = getContents >>= putStr . lastNLines 10

lastNLines :: Int -> String -> String
lastNLines n = unlines . takeLast n . lines

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse
