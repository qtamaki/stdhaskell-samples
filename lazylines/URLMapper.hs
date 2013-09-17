--
-- $Id: URLMapper.hs,v 1.3 2006/05/14 17:26:43 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module URLMapper
    (URLMapper, fromConfig, cgiURL, pageURL, pageEditURL) where

import URLEncoding
import Data.List
import Config

data URLMapper = URLMapper { cgiurl :: String,
                             rewrite :: Bool,
                             suffix :: String }

fromConfig :: Config -> URLMapper
fromConfig conf = URLMapper
    { cgiurl  = confLookupString "cgiurl" conf,
      rewrite = confLookupBool "rewrite" conf,
      suffix  = confLookupString "suffix" conf }

-- FIXME: URL auto detect?
cgiURL :: URLMapper -> String
cgiURL mapper = cgiurl mapper

pageURL :: URLMapper -> String -> String
pageURL (URLMapper cgiurl rewrite suffix) name
    | rewrite   = urlencode name ++ suffix
    | otherwise = cgiurl ++ "?name=" ++ urlencode name

pageEditURL :: URLMapper -> String -> String
pageEditURL (URLMapper { cgiurl = u }) name =
    u ++ "?cmd=edit;name=" ++ urlencode name
