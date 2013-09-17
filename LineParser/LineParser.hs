module LineParser
    (LineParser, firstChar, indented, blank, anyLine,
     parse, many, many1, eof) where

import MyParser
import Char (isSpace)

type LineParser a = MyParser String a

indented :: LineParser String
indented = firstChar isSpace

blank :: LineParser String
blank = satisfy (null . dropWhile isSpace)

firstChar :: (Char -> Bool) -> LineParser String
firstChar f = satisfy (test f)
  where
    test f ""    = False
    test f (c:_) = f c

anyLine :: LineParser String
anyLine = satisfy (\x -> True)
