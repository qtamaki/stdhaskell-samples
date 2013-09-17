--
-- $Id: Template.hs,v 1.3 2006/04/05 23:27:13 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Template
    (TemplateRepository, fromConfig, fillTemplate) where

import Config
import PathUtils
import Data.List
import Data.Char
import Data.Maybe

data TemplateRepository = TemplateRepository { prefix :: String }

fromConfig :: Config -> TemplateRepository
fromConfig conf =
    TemplateRepository { prefix = confLookupPath "directory" conf }

fillTemplate :: TemplateRepository -> String -> [(String,String)]
             -> IO String
fillTemplate repo id params = return . fill =<< loadTemplate repo id
  where
    fill ""   = ""
    fill tmpl = case break (== '$') tmpl of
                  (s, ('$':cs)) -> s ++ expand var ++ fill cont
                                   where (var, cont) = span isAlpha cs
                  (s, cont)     -> s ++ fill cont

    expand var = fromMaybe ('$':var) (lookup var params)

loadTemplate :: TemplateRepository -> String -> IO String
loadTemplate repo id = loadTemplate' id
  where
    loadTemplate' :: String -> IO String
    loadTemplate' id = procInclude =<< readTemplate id

    readTemplate :: String -> IO String
    readTemplate id = readFile $ joinPath (prefix repo) id

    procInclude :: String -> IO String
    procInclude cs = return . unlines =<< procInclude' (lines cs)

    procInclude' :: [String] -> IO [String]
    procInclude' []     = return []
    procInclude' (x:xs) = do ss <- expand x
                             xs' <- procInclude' xs
                             return (ss ++ xs')

    expand :: String -> IO [String]
    expand s = if ".include " `isPrefixOf` s
                   then procInclude' . lines =<< readTemplate (words s !! 1)
                   else return [s]
