--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

import System.FilePath (takeBaseName, takeDirectory, (</>), (<.>), dropFileName, takeFileName)
import System.Directory (listDirectory)
import qualified Data.Map as M


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "about/*.md" $ do
        route languageRoute
        compile $ do
          currentPath <- getResourceFilePath
          let lang = takeBaseName currentPath
          pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
            >>= relativizeUrls

    match "posts/**" $ do
        route languageRoute
        compile $ do
          currentPath <- getResourceFilePath
          let lang = takeBaseName currentPath
          paths <- unsafeCompiler . getAbsoluteFilepaths . takeDirectory . drop 2 $ currentPath
          let i18nItems = buildI18nItems paths
          pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx (return i18nItems))
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
            >>= relativizeUrls

    indexRules "de"
    indexRules "en"
    indexRules "ko"




--------------------------------------------------------------------------------

indexRules :: String -> Rules ()
indexRules lang =
    match "templates/index.html" $ version lang $ do
      route . templateI18nRoute $ lang
      compile $ do
        posts <- recentFirst =<< loadAll (fromGlob ("posts/*/" ++ lang ++ ".md"))
        getResourceBody
            >>= applyAsTemplate (indexCtx lang posts)
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
            >>= relativizeUrls

indexCtx :: String -> [ Item String ] -> Context String
indexCtx lang posts =
    listField "posts" postsCtx (return posts) `mappend`
    constField "postsHeader" (postsHeader lang) `mappend`
    defaultContext

headerCtx :: String -> Context String
headerCtx lang =
    listField "langs" indexI18nCtx (return . buildI18nItems $ supportedLangs) `mappend`
    constField "homeLinkText" (homeLinkText lang) `mappend`
    constField "aboutLinkText" (aboutLinkText lang) `mappend`
    constField "title" "Harfang's Perch" `mappend`
    defaultContext

supportedLangs :: [ FilePath ]
supportedLangs =
    [ "en", "ko", "de" ]

indexLinkUrl :: String -> String
indexLinkUrl lang =
  "/" ++ lang ++ "/index.html"

indexLinkText :: String -> String
indexLinkText lang =
  case lang of
    "en" -> "Posts"
    "ko" -> "글 목록"
    "de" -> "Posts"
    _ -> "Posts"

postsHeader :: String -> String
postsHeader lang =
  case lang of
    "en" -> "Posts"
    "ko" -> "글 목록"
    "de" -> "Posts"
    _ -> "Posts"

aboutLinkText :: String -> String
aboutLinkText lang =
  case lang of
    "en" -> "About"
    "ko" -> "소개"
    "de" -> "About"
    _ -> "About"

homeLinkText :: String -> String
homeLinkText lang =
  case lang of
    "en" -> "Home"
    "ko" -> "홈"
    "de" -> "Home"
    _ -> "Home"

templateI18nRoute :: String -> Routes
templateI18nRoute lang =
  customRoute . templateI18nRouteBuilder $ lang

languageRoute :: Routes
languageRoute =
  customRoute identifierToUrl

postCtx :: Compiler [ Item String ] -> Context String
postCtx i18nUrls =
    listField "i18nUrls" postI18nCtx i18nUrls `mappend`
    dateField "date" "%F" `mappend`
    defaultContext

postsCtx :: Context String
postsCtx =
    teaserField "teaser" "content" `mappend`
    dateField "date" "%F" `mappend`
    defaultContext

postI18nCtx :: Context String
postI18nCtx =
    (field "langUrl" $ \item -> return . (++) "/" . identifierToUrl . itemIdentifier $ item ) `mappend`
    (field "langName" $ \item -> return . takeBaseName . toFilePath . itemIdentifier $ item)

indexI18nCtx :: Context String
indexI18nCtx =
    (field "langUrl" $ \item -> return . indexLinkUrl . toFilePath . itemIdentifier $ item ) `mappend`
    (field "langName" $ \item -> return . indexLinkText . takeBaseName . toFilePath . itemIdentifier $ item)

templateI18nRouteBuilder :: String -> Identifier -> FilePath
templateI18nRouteBuilder lang id =
  lang </> (takeFileName . toFilePath $ id)

identifierToUrl :: Identifier -> FilePath
identifierToUrl id =
    let path = toFilePath id
    in
        takeBaseName path </> takeDirectory path <.> "html"

buildI18nItems :: [ FilePath ] -> [ Item String ]
buildI18nItems =
  map (\path ->
         Item { itemIdentifier = fromFilePath path
              , itemBody = ""}
         )

getAbsoluteFilepaths :: FilePath -> IO [ FilePath ]
getAbsoluteFilepaths path = do
  contents <- listDirectory path
  return $ map (path </>) contents
