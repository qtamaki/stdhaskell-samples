--
-- $Id: LazyLines.hs,v 1.4 2006/04/09 11:13:21 aamine Exp $
--
-- Copyright (c) 2005,2006 Minero Aoki
--
-- This program is free software.
-- You can distribute/modify this program under the terms of
-- the GNU LGPL, Lesser General Public License version 2.1.
-- For details of the GNU LGPL, see the file "COPYING".
--

module LazyLines (Context(..), loadContext, appMain) where

import Config
import CGI
import Database
import Syntax
import HTML hiding (ol, li)
import Template
import URLMapper
import URLEncoding
import Data.List
import Data.Maybe
import System.Time
import System.Locale (defaultTimeLocale)

frontPageName = "FrontPage"

appMain :: Context -> HTTPRequest -> IO HTTPResponse
appMain ctx = wikiSession ctx . wikiRequest

data Context = Context Database TemplateRepository URLMapper

loadContext :: String -> IO Context
loadContext path =
    do conf <- loadConfig path
       return $ Context (Database.fromConfig (confLookup "database" conf))
                        (Template.fromConfig (confLookup "template" conf))
                        (URLMapper.fromConfig (confLookup "urlmapper" conf))

data WikiRequest = ViewRequest { name :: String }
                 | EditRequest { name :: String }
                 | SaveRequest { name :: String, content :: String }
                 | RecentRequest

wikiRequest :: HTTPRequest -> WikiRequest
wikiRequest req =
    case (lookupVar "cmd" req, lookupVar "name" req) of
      (Nothing,     Just name) -> ViewRequest name
      (Just "view", Just name) -> ViewRequest name
      (Just "edit", Just name) -> EditRequest name
      (Just "save", Just name) -> case lookupVar "text" req of
                                    Just cs -> SaveRequest name cs
                                    Nothing -> ViewRequest frontPageName
      (Just "recent", _)       -> RecentRequest
      _ -> ViewRequest frontPageName

wikiSession :: Context -> WikiRequest -> IO HTTPResponse
wikiSession (Context db tmpl umap) req =
    catch (respondTo req) (\err -> frontPageResponse)
  where
    respondTo (ViewRequest name) =
        catch (viewPageResponse name) (\err -> editPageResponse name)

    respondTo (EditRequest name) = editPageResponse name

    respondTo (SaveRequest name content) =
        do savePageSource db name content
           return $ softRedirectResponse (pageURL umap name)

    respondTo (RecentRequest) = recentResponse

    frontPageResponse = viewPageResponse frontPageName

    viewPageResponse name =
        do body <- pageHTML db name
           html <- fill "view" name body
           return (HTTPResponse pageContentType html)

    editPageResponse name =
        do text <- catch (pageSource db name) (\err -> return "")
           html <- fill "edit" name (escape text)
           return (HTTPResponse pageContentType html)

    recentResponse =
        do body <- return . makeRecentPage . sortBy newerFirst
                                           =<< pageNamesWithMtime db
           html <- fillTemplate tmpl "recent"
               [ ("cgiURL", escape (cgiURL umap)),
                 ("encoding", pageEncoding db),
                 ("content", body) ]
           return (HTTPResponse pageContentType html)

    newerFirst (_,a) (_,b) = b `compare` a

    makeRecentPage = ol . unlines . map makeRecentPageCol

    makeRecentPageCol (name, mtime) =
        li (formatTime mtime ++ ": " ++ pageAnchor umap name)

    pageAnchor umap name = a_href (pageURL umap name) name

    pageContentType = textContentType "text/html" $ pageEncoding db

    pageHTML db name = do text <- pageSource db name
                          return $ htmlToText href (compile text)

    href (PageLink name) = a_href (pageURL umap name) name

    fill id name content = fillTemplate tmpl id
        [ ("cgiURL", escape (cgiURL umap)),
          ("encoding", pageEncoding db),
          ("pageName", escape name),
          ("encodedPageName", escape (urlencode name)),
          ("content", content) ]

    softRedirectResponse url = HTTPResponse "text/html" $ concat $
        [ "<html>\n",
          "<head>\n",
          "<meta http-equiv=\"Refresh\" content=\"1;url=", escape url, "\">\n",
          "<title>Redirect</title>\n",
          "</head>\n",
          "<body>\n",
          "<p><a href=\"", escape url, "\">Redirect: ", escape url,"</a></p>\n",
          "</body>\n",
          "</html>\n" ]

formatTime = strftime "%Y-%m-%d %H:%M:%S %Z"

strftime fmt = formatCalendarTime defaultTimeLocale fmt

ol s = "<ol>\n" ++ s ++ "</ol>"
li s = "<li>" ++ s ++ "</li>"
