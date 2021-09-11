module I18n where

import System.FilePath (takeBaseName, takeDirectory, (</>), (<.>))
import Hakyll

-- Language Configurations

mainLangs :: [ FilePath ]
mainLangs =
    [ "en", "ko" ]

allLangs :: [ FilePath ]
allLangs =
    [ "en", "ko", "de" ]

languageName :: String -> String
languageName lang =
  case lang of
    "en" -> "English"
    "ko" -> "한국어"
    "de" -> "Deutsch"
    _ -> "Unknown Language"

-- Link Texts and Link Urls

indexLinkUrl :: String -> FilePath
indexLinkUrl lang =
  lang ++ "/index.html"

indexLinkText :: String -> String
indexLinkText lang =
  case lang of
    "en" -> "Posts"
    "ko" -> "글 목록"
    "de" -> "Posts"
    _ -> "Posts"

postLinkUrl :: FilePath -> FilePath
postLinkUrl path =
  takeBaseName path </> takeDirectory path <.> "html"

aboutLinkUrl :: String -> String
aboutLinkUrl lang =
  "/" ++ lang ++ "/about.html"

aboutLinkText :: String -> String
aboutLinkText lang =
  case lang of
    "en" -> "About"
    "ko" -> "소개"
    "de" -> "About"
    _ -> "About"

postsLinkUrl :: String -> String
postsLinkUrl lang =
  "/" ++ lang ++ "/index.html"

postsLinkText :: String -> String
postsLinkText lang =
  case lang of
    "en" -> "Posts"
    "ko" -> "글 목록"
    "de" -> "Posts"
    _ -> "Posts"

readMoreLinkText :: String -> String
readMoreLinkText lang =
  case lang of
    "en" -> "Read more"
    "ko" -> "전체 보기"
    "de" -> "Weiter lesen"
    _ -> "Read more"

-- Feed Urls

rssFeedUrl :: String -> String
rssFeedUrl lang =
  "/" ++ lang ++ "/rss.xml"

atomFeedUrl :: String -> String
atomFeedUrl lang =
  "/" ++ lang ++ "/atom.xml"

-- Helper

emptyLanguageItems :: [ FilePath ] -> [ Item String ]
emptyLanguageItems =
  map (\path ->
         Item { itemIdentifier = fromFilePath path
              , itemBody = ""}
         )
