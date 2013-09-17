--
-- $original Id: PathUtils.hs,v 1.9 2006/03/06 13:42:21 aamine Exp $
--
-- Copyright (c) 2003-2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LPGL, see the file "COPYING".
--

module PathUtils
    (concatPath, joinPath, ancestors, dirname, basename) where

import Data.List

#ifdef POSIX
pathSep = '/'
pathSepStr = "/"
#elif WIN32
pathSep = '\\'
pathSepStr = "\\"
#endif

rootPath = pathSepStr

isRoot :: String -> Bool
isRoot = (== rootPath)

concatPath :: [String] -> String
concatPath [] = error "concatPath []"
concatPath cs = foldl1 joinPath cs

joinPath :: String -> String -> String
joinPath a b
    | null a            = pathSep : b
    | last a == pathSep = a ++ b
    | otherwise         = a ++ pathSepStr ++ b

ancestors :: String -> [String]
ancestors path | isRoot path = [rootPath]
               | otherwise   = path : ancestors' (dirname path)
  where
    ancestors' path | path == "." = []
                    | isRoot path = [rootPath]
                    | otherwise   = path : ancestors' (dirname path)

dirname :: String -> String
dirname = fst . splitPath

basename :: String -> String
basename = snd . splitPath

splitPath :: String -> (String, String)
splitPath ""   = error "splitPath \"\""
splitPath path = case splitPath' path of
                   ("", (_:[])) -> (pathSepStr, pathSepStr)
                   (b,  ""    ) -> (".",        reverse b)
                   (b,  (_:[])) -> (pathSepStr, reverse b)
                   (b,  (_:d) ) -> (reverse d,  reverse b)
  where
    splitPath' = break (== pathSep) . dropSep . uniqSeps . reverse

    dropSep path | isRoot path = rootPath
                 | otherwise   = dropWhile (== pathSep) path

    uniqSeps ""   = ""
    uniqSeps path = let ps = group path
                    in if head (head ps) == pathSep
                       then pathSep : concat (tail ps)
                       else path
