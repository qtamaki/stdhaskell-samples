import List

{-
data Line = Line Int String  deriving Show
-}
data Line = Line { number :: Int, string :: String }  deriving Show

myLines :: [Line]
myLines = [ (Line 4 "4th line"),
            (Line 1 "first line"),
            (Line 5 "5th line"),
            (Line 3 "3rd line"),
            (Line 2 "second line") ]

sortLines :: [Line] -> [Line]
sortLines = sortBy (\a b -> number a `compare` number b)

main = do
          print $ Line 1 "first line"
          print $ myLines
          print $ sortLines myLines
