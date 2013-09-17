--
-- $Id: FileUtils.hs,v 1.2 2006/04/05 17:55:14 aamine Exp $
--
-- Copyright (c) 2003-2005 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LPGL, see the file "COPYING".
--

module FileUtils where

import PathUtils
import Control.Monad
import System.IO
import System.IO.Error
import System.Directory

forceRemove :: FilePath -> IO ()
forceRemove path = catch (removeFile path) (const $ return ())

fileEntries :: FilePath -> IO [String]
fileEntries path = filterM (notFile) =<< dirEntries path
  where
    notFile name = doesFileExist (joinPath path name)

dirEntries :: FilePath -> IO [String]
dirEntries path = return . filter notDotFile =<< getDirectoryContents path
  where
    notDotFile ('.':_)  = False
    notDotFile _        = True

makePath :: FilePath -> IO ()
makePath = mkdirs . ancestors
  where
    mkdirs :: [String] -> IO ()
    mkdirs [] = return ()
    mkdirs (x:xs) =
        catch (mkdir_f x)
              (\ex -> if isDoesNotExistError ex then
                          mkdirs xs >>
                          mkdir_f x
                      else
                          ioError ex)

    mkdir_f path =
        catch (createDirectory path)
              (\ex -> if isAlreadyExistsError ex then
                          return ()
                      else
                          ioError ex)
