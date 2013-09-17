main = do cs <- getContents
          putStr $ numbering cs

numbering :: String -> String
numbering = unlines . map format . zipLineNumber . lines

zipLineNumber :: [String] -> [(Int, String)]
zipLineNumber = zip [1..]

format :: (Int, String) -> String
format (n, line) = rjust 6 (show n) ++ "  " ++ line

rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s
