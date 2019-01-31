--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Control.Monad (liftM)
import System.FilePath (takeBaseName, takeDirectory, (</>), takeFileName)
import System.Directory (listDirectory)

import Hakyll
import I18n
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/main.scss" $ do
        route   $ constRoute "main.css"
        compile compressScssCompiler

    match "templates/*" $ compile templateBodyCompiler

    match "about/*.md" $ do
        route postRoute
        compile $ do
          currentPath <- getResourceFilePath
          let lang = takeBaseName currentPath
          pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" (defaultCtx lang)
            >>= relativizeUrls

    match "posts/**" $ do
        route postRoute
        compile $ do
          currentPath <- getResourceFilePath
          let lang = takeBaseName currentPath
          paths <- unsafeCompiler . getAbsoluteFilepaths . takeDirectory . drop 2 $ currentPath
          let i18nItems = emptyLanguageItems paths
          pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx (return i18nItems))
            >>= loadAndApplyTemplate "templates/default.html" (defaultCtx lang)
            >>= relativizeUrls

    paginateEn <- buildPaginateWith postsGrouper (postsPattern "en") (postsPageId "en")
    indexRules "en" paginateEn
    paginateDe <- buildPaginateWith postsGrouper (postsPattern "de") (postsPageId "de")
    indexRules "de" paginateDe
    paginateKo <- buildPaginateWith postsGrouper (postsPattern "ko") (postsPageId "ko")
    indexRules "ko" paginateKo
--------------------------------------------------------------------------------

indexRules :: String -> Paginate -> Rules ()
indexRules lang paginate =
    paginateRules paginate $ \pageNumber pattern -> do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" (indexCtx paginate pageNumber lang posts)
              >>= loadAndApplyTemplate "templates/default.html" (defaultCtx lang)
              >>= relativizeUrls

compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
  fmap (fmap compressCss) $
    getResourceString
    >>= withItemBody (unixFilter "sass" [ "-s"
                                        , "--scss"
                                        , "--style", "compressed"
                                        , "--load-path", "css"
                                        ])

-- Contexts

indexCtx :: Paginate -> PageNumber -> String -> [ Item String ] -> Context String
indexCtx paginate pageNumber lang posts =
    listField "posts" (postsCtx lang) (return posts) `mappend`
    constField "postsHeader" (languageName lang) `mappend`
    constField "title" "Harfang's Perch" `mappend`
    paginateContext paginate pageNumber `mappend`
    defaultContext

postCtx :: Compiler [ Item String ] -> Context String
postCtx i18nUrls =
    listField "i18nUrls" (i18nCtx postLinkUrl languageName) i18nUrls `mappend`
    dateField "date" "%F" `mappend`
    defaultContext

postsCtx :: String -> Context String
postsCtx lang =
    teaserField "teaser" "content" `mappend`
    dateField "date" "%F" `mappend`
    constField "readMoreLinkText" (readMoreLinkText lang) `mappend`
    defaultContext

defaultCtx :: String -> Context String
defaultCtx lang =
    listField "langs" (i18nCtx indexLinkUrl languageName) (return . emptyLanguageItems $ supportedLangs) `mappend`
    constField "postsLinkText" (postsLinkText lang) `mappend`
    constField "postsLinkUrl" (postsLinkUrl lang) `mappend`
    constField "aboutLinkText" (aboutLinkText lang) `mappend`
    constField "aboutLinkUrl" (aboutLinkUrl lang) `mappend`
    constField "htmlLang" lang <>
    constField "title" "Harfang's Perch" `mappend`
    defaultContext

i18nCtx :: (FilePath -> FilePath) -> (String -> String) -> Context String
i18nCtx urlTransformer textTransformer =
    (field "langUrl" $ \item -> return $ "/" ++ (urlTransformer . toFilePath . itemIdentifier $ item )) `mappend`
    (field "langName" $ \item -> return . textTransformer . takeBaseName . toFilePath . itemIdentifier $ item)

-- Routes
postRoute :: Routes
postRoute =
  customRoute (\identifier -> postLinkUrl . toFilePath $ identifier)

-- Helpers
emptyLanguageItems :: [ FilePath ] -> [ Item String ]
emptyLanguageItems =
  map (\path ->
         Item { itemIdentifier = fromFilePath path
              , itemBody = ""}
         )

getAbsoluteFilepaths :: FilePath -> IO [ FilePath ]
getAbsoluteFilepaths path = do
  contents <- listDirectory path
  return $ map (path </>) contents

-- Paginate
postsPageId :: String -> PageNumber -> Identifier
postsPageId lang n = fromFilePath $ if (n == 1) then lang </> "index.html" else lang </> show n </> "index.html"

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 10) . sortRecentFirst

postsPattern :: String -> Pattern
postsPattern lang =
  (fromGlob ("posts/*/" ++ lang ++ ".md"))
