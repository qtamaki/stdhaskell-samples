foldWidth = 60

main = do cs <- getContents
          putStr $ fold cs

fold cs = unlines $ concatMap foldLine $ lines cs

foldLine line = case splitAt foldWidth line of
                  (s, [])   -> [s]
                  (s, cont) -> s : foldLine cont
