main = do cs <- getContents
          putStr $ numbering cs

numbering :: String -> String
numbering cs = unlines $ map format $ zipLineNumber $ lines cs

zipLineNumber :: [String] -> [(Int, String)]
zipLineNumber xs = zip [1..] xs

format :: (Int, String) -> String
format (n, line) = rjust 6 (show n) ++ "  " ++ line

rjust :: Int -> String -> String
rjust width s = replicate (width - length s) ' ' ++ s
