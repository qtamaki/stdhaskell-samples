--
-- $Id: CGIMain.hs,v 1.1 2006/04/05 18:38:26 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Main (main) where

import CGI
import LazyLines

main = do ctx <- loadContext "./config"
          runCGI (appMain ctx)
