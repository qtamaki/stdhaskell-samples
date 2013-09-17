tabStop = 8

main :: IO ()
main = getContents >>= putStr . concatMap expandTab

expandTab :: Char -> String
expandTab '\t' = replicate tabStop ' '
expandTab c    = [c]
