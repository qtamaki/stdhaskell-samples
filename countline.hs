main = do cs <- getContents
          print $ length $ lines cs
