import List

main = putStr . unlines . map head . group . lines =<< getContents
