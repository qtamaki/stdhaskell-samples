main = do cs <- getContents
          putStr $ lastNLines 10 cs

lastNLines :: Int -> String -> String
lastNLines n cs = unlines $ takeLast n $ lines cs

takeLast :: Int -> [String] -> [String]
takeLast n xs = diffList xs (drop n xs)

diffList :: [String] -> [String] -> [String]
diffList xs []         = xs
diffList [] ys         = ys
diffList (_:xs) (_:ys) = diffList xs ys
