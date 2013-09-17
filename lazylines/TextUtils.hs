--
-- $Id: TextUtils.hs,v 1.3 2006/04/05 17:55:14 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module TextUtils (strip, lstrip, rstrip, isBlank) where

import Data.Char

strip :: String -> String
strip  = rstrip . lstrip

lstrip :: String -> String
lstrip = dropWhile isSpace

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

isBlank :: String -> Bool
isBlank = all isSpace
