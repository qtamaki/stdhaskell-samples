import List

main = do cs <- getContents
          putStr $ unlines $ map head $ group $ lines cs
