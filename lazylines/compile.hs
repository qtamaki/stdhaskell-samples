--
-- $Id: compile.hs,v 1.4 2006/04/09 11:08:56 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Main (main) where

import Syntax
import HTML

main = interact (htmlToText href . compile)

href (PageLink name) = a_href ("$pageURL(" ++ name ++ ")") name
