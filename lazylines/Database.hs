--
-- $Id: Database.hs,v 1.3 2006/04/05 23:27:13 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module Database
    (Database, encoding, fromConfig,
     pageSource, pageEncoding, savePageSource, doesPageExist,
     pageNames, pageNamesWithMtime) where

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Config
import URLEncoding
import FileUtils
import PathUtils
import Data.List ()
import Control.Monad
import Control.Concurrent (threadDelay)
import System.IO
import System.IO.Error
import System.Directory
import System.Time
#ifdef _POSIX
import Data.Time (UTCTime)
import Control.Exception (bracket)
import System.Posix.IO
#endif

data Database = Database { prefix :: String, encoding :: String }

fromConfig :: Config -> Database
fromConfig conf =
    Database { prefix = confLookupPath "directory" conf,
               encoding = confLookupString "encoding" conf }

encodeName = urlencode
decodeName = urldecode

pagePath db name = concatPath [prefix db, "pages", encodeName name]

pageSource :: Database -> String -> IO String
pageSource db name = readFile (pagePath db name)

pageEncoding :: Database -> String
pageEncoding db = encoding db

doesPageExist :: Database -> String -> IO Bool
doesPageExist db name = doesFileExist (pagePath db name)

pageNames :: Database -> IO [String]
pageNames db = return . map decodeName =<< fileEntries (prefix db ++ "/pages")

pageNamesWithMtime :: Database -> IO [(String, CalendarTime)]
pageNamesWithMtime db =
    do names <- pageNames db
       return . zip names =<< mapM (mtime . pagePath db) names

mtime :: String -> IO CalendarTime
mtime path = toCalendarTime . uToC =<< getModificationTime path

uToC :: UTCTime -> ClockTime
uToC t = TOD (truncate . utcTimeToPOSIXSeconds $ t) 0

nRetry = 5

#if WIN32
savePageSource :: Database -> String -> String -> IO ()
savePageSource (Database { prefix = dir }) name content =
    do let destdir = joinPath dir "pages"
           destpath = joinPath destdir (encodeName name)
       makePath destdir
       retryWhile isAlreadyExistsError
           $ replicate nRetry (writeFile destpath content)
#elif _POSIX
savePageSource :: Database -> String -> String -> IO ()
savePageSource (Database { prefix = dir, encoding = enc }) name content =
    do let tmpdir = joinPath dir "tmp/pages"
           destdir = joinPath dir "pages"
       makePath tmpdir
       makePath destdir
       let tmppath = joinPath tmpdir (encodeName name)
           destpath = joinPath destdir (encodeName name)
       atomicWriteFile tmppath destpath enc content

atomicWriteFile :: FilePath -> FilePath -> String -> String -> IO ()
atomicWriteFile tmppath destpath enc content =
    do retryWhile isAlreadyExistsError
           $ replicate nRetry $ (exclWriteFile tmppath content)
       catchIOError (renameFile tmppath destpath)
             (\err -> do forceRemove tmppath
                         ioError err)
  where

    exclWriteFile path content = bracket (fdToHandle =<< exclCreate path)
                                         (hClose)
                                         (\h -> do en <- mkTextEncoding enc
                                                   hSetEncoding h en
                                                   hPutStr h content)

    exclCreate path = openFd path WriteOnly (Just 0o666)
                          (defaultFileFlags { exclusive = True })
#endif

retryWhile f []     = ioError (userError "failed to lock file")
retryWhile f (x:xs) = catchIOError (x) (\err -> do unless (f err) (ioError err)
                                                   threadDelay (10^6)
                                                   retryWhile f xs)
