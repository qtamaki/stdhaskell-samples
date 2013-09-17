--
-- $Id: HTML.hs,v 1.5 2006/04/15 00:23:34 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module HTML
    (HTML, HTMLItem(..), Href(..), htmlToText,
     h, ul, ol, li, dl, dt, dd, pre, p, a_href,
     escape, escapeChar) where

type HTML = [HTMLItem]

data HTMLItem = Text String | Param Href

data Href = PageLink { hrefPageName :: String }

htmlToText :: (Href -> String) -> HTML -> String
htmlToText f = concatMap resolve
  where
    resolve (Text x)  = x
    resolve (Param x) = f x

h :: Int -> HTML -> HTML
h n = joinWithTag ('h' : show level) False
      where level = max 1 (min 6 n)

ul, ol, li, dl, dt, dd, pre, p :: HTML -> HTML
ul  = joinWithTag "ul"  True
ol  = joinWithTag "ol"  True
li  = joinWithTag "li"  False
dl  = joinWithTag "dl"  True
dt  = joinWithTag "dt"  False
dd  = joinWithTag "dd"  False
pre = joinWithTag "pre" False
p   = joinWithTag "p"   False

joinWithTag :: String -> Bool -> HTML -> HTML
joinWithTag name addEOL content =
    Text ("<"++name++">" ++ eol) : (content ++ [Text ("</"++name++">\n")])
  where
    eol = if addEOL then "\n" else ""

a_href :: String -> String -> String
a_href url label =
    "<a href=\"" ++ escape url ++ "\">" ++ escape label ++ "</a>"

escape :: String -> String
escape = concatMap escapeChar

escapeChar :: Char -> String
escapeChar '<' = "&lt;"
escapeChar '>' = "&gt;"
escapeChar '&' = "&amp;"
escapeChar '"' = "&quot;"
escapeChar c = [c]
