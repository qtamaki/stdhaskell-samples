--
-- $Id: Config.hs,v 1.2 2006/04/05 17:55:14 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Config
    (Config, loadConfig, parseConfigFile, confLookup,
     confLookupString, confLookupPath, confLookupBool) where

import TextUtils
import Data.List
import Data.Maybe

type Config = [(String, String)]

confLookup :: Eq a => a -> [(a, b)] -> b
confLookup key = fromJust . lookup key

confLookupString :: String -> Config -> String
confLookupString = confLookup

confLookupPath :: String -> Config -> FilePath
confLookupPath = confLookupString

confLookupBool :: String -> Config -> Bool
confLookupBool key = read . confLookup key

loadConfig :: FilePath -> IO [(String, Config)]
loadConfig path = return . parseConfigFile =<< readFile path

parseConfigFile :: String -> [(String, Config)]
parseConfigFile = map reduce . categorize . concatMap parseLine . lines
  where
    reduce :: Config -> (String, Config)
    reduce kvs = (category . fst . head $ kvs,
                  map (\(k,v) -> (itemname k, v)) kvs)

    categorize :: Config -> [Config]
    categorize = groupBy (\(a,_) (b,_) -> category a == category b)

    -- a key of the config item = "category.itemname"
    category = fst . break (== '.')
    itemname = tail . snd . break (== '.')

    parseLine = parseLinePlain . dropComment

    parseLinePlain line
        | isBlank line = []
        | otherwise    = let (k, ('=':v)) = break (== '=') . strip $ line
                         in [(strip k, strip v)]

    dropComment = fst . break (== '#')
