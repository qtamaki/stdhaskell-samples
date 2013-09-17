main = do cs <- getContents
          putStr $ lastNLines 10 cs

lastNLines n cs = unlines $ takeLast n $ lines cs

takeLast n ss = reverse $ take n $ reverse ss
