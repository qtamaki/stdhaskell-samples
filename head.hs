main = do cs <- getContents
          putStr $ firstNLines 10 cs

firstNLines n cs = unlines $ take n $ lines cs
