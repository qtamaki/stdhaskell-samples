--
-- $Id: LineParser.hs,v 1.4 2006/04/09 20:40:24 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module LineParser
    (module Text.ParserCombinators.Parsec.Prim,
     module Text.ParserCombinators.Parsec.Combinator,
     LineParser, indented, blank, firstChar, anyLine) where

import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim
import Text.ParserCombinators.Parsec.Pos
import Data.Char (isSpace)

type LineParser a = GenParser String () a

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
anyLine = satisfy (const True)

satisfy :: (String -> Bool) -> LineParser String
satisfy f = tokenPrim (\s -> show s)
                      (\pos s ss -> incSourceLine pos 1)
                      (\s -> if f s then Just s else Nothing)
