main = do cs <- getContents
          putStr $ tac cs

tac cs = unlines $ reverse $ lines cs
