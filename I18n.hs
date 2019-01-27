module I18n where

import System.FilePath (takeBaseName, takeDirectory, (</>), (<.>))

supportedLangs :: [ FilePath ]
supportedLangs =
    [ "en", "ko" ]

indexLinkUrl :: String -> FilePath
indexLinkUrl lang =
  "/" ++ lang ++ "/index.html"

indexLinkText :: String -> String
indexLinkText lang =
  case lang of
    "en" -> "Posts"
    "ko" -> "글 목록"
    "de" -> "Posts"
    _ -> "Posts"

postLinkUrl :: FilePath -> FilePath
postLinkUrl path =
  "" </> takeBaseName path </> takeDirectory path <.> "html"

languageName :: String -> String
languageName lang =
  case lang of
    "en" -> "English"
    "ko" -> "한국어"
    "de" -> "Deutsch"
    _ -> "Unknown Language"

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
    _ -> "Home"
