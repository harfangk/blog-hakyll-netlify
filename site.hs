--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import I18n

import System.FilePath (takeBaseName, takeDirectory, (</>), takeFileName)
import System.Directory (listDirectory)


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
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
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
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
            >>= relativizeUrls

    indexRules "de"
    indexRules "en"
    indexRules "ko"
--------------------------------------------------------------------------------

indexRules :: String -> Rules ()
indexRules lang =
    match "templates/index.html" $ version lang $ do
      route . templateRoute $ lang
      compile $ do
        posts <- recentFirst =<< loadAll (fromGlob ("posts/*/" ++ lang ++ ".md"))
        getResourceBody
            >>= applyAsTemplate (indexCtx lang posts)
            >>= loadAndApplyTemplate "templates/default.html" (headerCtx lang)
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

indexCtx :: String -> [ Item String ] -> Context String
indexCtx lang posts =
    listField "posts" (postsCtx lang) (return posts) `mappend`
    constField "postsHeader" (languageName lang) `mappend`
    defaultContext

headerCtx :: String -> Context String
headerCtx lang =
    listField "langs" (i18nCtx indexLinkUrl languageName) (return . emptyLanguageItems $ supportedLangs) `mappend`
    constField "postsLinkText" (postsLinkText lang) `mappend`
    constField "postsLinkUrl" (postsLinkUrl lang) `mappend`
    constField "aboutLinkText" (aboutLinkText lang) `mappend`
    constField "aboutLinkUrl" (aboutLinkUrl lang) `mappend`
    constField "title" "Harfang's Perch" `mappend`
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

i18nCtx :: (FilePath -> FilePath) -> (String -> String) -> Context String
i18nCtx urlTransformer textTransformer =
    (field "langUrl" $ \item -> return $ "/" ++ (urlTransformer . toFilePath . itemIdentifier $ item )) `mappend`
    (field "langName" $ \item -> return . textTransformer . takeBaseName . toFilePath . itemIdentifier $ item)

-- Routes

templateRoute :: String -> Routes
templateRoute lang =
  customRoute (\identifier -> lang </> (takeFileName . toFilePath $ identifier))

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
