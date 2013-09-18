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
import GHC.IO.Encoding
import System.IO
import Database

main = do ctx <- loadContext "./config"
          enc <- getEnc ctx
          hSetEncoding stdout enc
          hSetEncoding stdin enc
          runCGI (appMain ctx)

getEnc :: Context -> IO TextEncoding
getEnc (Context db _ _) = mkTextEncoding $ encoding db


