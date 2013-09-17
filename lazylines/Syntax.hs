--
-- $Id: Syntax.hs,v 1.9 2006/04/23 13:06:46 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Syntax (compile) where

import LineParser
import HTML
import TextUtils
import Text.ParserCombinators.Parsec
import Data.List
import Data.Char

compile :: String -> HTML
compile str = case parse document "" (lines str) of
                Right html -> html
                Left err -> p [Text (escape $ show err)]

document :: LineParser HTML
document = do htmls <- many block
              eof
              return (concat htmls)

block :: LineParser HTML
block  =  do many1 blank
             return []
      <|> headline
      <|> ulist
      <|> olist
      <|> dlist
      <|> preformatted
      <|> paragraph

headline :: LineParser HTML
headline = do line <- firstChar (== '=')
              let (mark, label) = span (== '=') line
              return $ h (length mark) [Text (escape $ strip label)]

ulist :: LineParser HTML
ulist = do items <- many1 item
           return $ ul (concat items)
  where
    item = do line <- firstChar (== '*')
              return (li . compileText . strip . tail $ line)

olist :: LineParser HTML
olist = do items <- many1 item
           return $ ol (concat items)
  where
    item = do line <- firstChar (== '#')
              return (li . compileText . strip . tail $ line)

dlist :: LineParser HTML
dlist = do lines <- many1 (firstChar (== ':'))
           return $ dl (concatMap compileItem lines)
  where
    compileItem str = case break (==':') (tail str) of
                        (t,(':':d)) -> compileDT t ++ compileDD d
                        (t,      _) -> compileDT t

    compileDT = dt . compileText . strip
    compileDD = dd . compileText . strip

preformatted :: LineParser HTML
preformatted = do lines <- many1 indented
                  return $ pre [Text . escape . join . unindentBlock $ lines]

paragraph :: LineParser HTML
paragraph = do line <- anyLine
               lines <- many (firstChar isNoFunc)
               return (p . compileText . join $ (line:lines))
  where
    isNoFunc = (`notElem` "=*#: \t\r\n\v\f")

join :: [String] -> String
join = concat . intersperse "\n"

tabstop = 8

unindentBlock :: [String] -> [String]
unindentBlock lines = map (unindent n) lines
                      where n = minimum (map numIndent lines)

unindent :: Int -> String -> String
unindent n line = let (spaces, str) = span isSpace line
                  in drop n (untabify tabstop spaces) ++ str

numIndent :: String -> Int
numIndent = length . untabify tabstop . takeWhile isSpace

untabify :: Int -> String -> String
untabify ts = concatMap expandTab
  where
    expandTab '\t' = replicate ts ' '
    expandTab c    = [c]

compileText :: String -> HTML
compileText str = case parse text "" str of
                    Right html -> html
                    Left err -> [Text (escape $ show err)]

text :: Parser HTML
text = many component
  where
    component =  do name <- try(wikiName)
                    return (Param $ PageLink name)
             <|> do url <- try(urlAutoLink)
                    return (Text $ a_href url url)
             <|> do c <- anyChar
                    return (Text $ escapeChar c)

wikiName :: Parser String
wikiName = do w1 <- word
              w2 <- word
              ws <- many word
              return (concat (w1:w2:ws))
  where
    word = do c <- upper
              s <- many1 (lower <|> digit)
              return (c:s)

urlAutoLink :: Parser String
urlAutoLink = do a <- string "http"
                 b <- option "" (string "s")
                 c <- string "://"
                 d <- many1 urlChar
                 return $ concat [a,b,c,d]

urlChar :: Parser Char
urlChar =  alphaNum
       <|> oneOf ";/?:@&=+$,-_.!~*'#%"  -- '(' ')'
