--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Control.Monad (liftM, mapM)
import System.FilePath (takeBaseName, (</>), (<.>))
import System.Directory (listDirectory, copyFile)

import Hakyll
import qualified I18n
--------------------------------------------------------------------------------
main :: IO ()
main = do
  copyFile "./_redirects" "./_site/_redirects"

  filesPerPostList <- listFilesPerPost

  hakyll $ do
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

    foldl1 (>>) (map postRules filesPerPostList)

    foldl1 (>>) (map indexRules I18n.allLangs)

    let feedItems = cartProd I18n.mainLangs ["rss", "atom"]

    foldl1 (>>) (map feedRules feedItems)

--------------------------------------------------------------------------------
-- Rules

indexRules :: String -> Rules ()
indexRules lang = do
    paginate <- buildPaginateWith postsGrouper (postsPattern lang) (postsPageId lang)
    paginateRules paginate $ \pageNumber pattern -> do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll pattern
          makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" (indexCtx paginate pageNumber lang posts)
              >>= loadAndApplyTemplate "templates/default.html" (defaultCtx lang)
              >>= relativizeUrls

postRules :: (FilePath, [FilePath]) -> Rules ()
postRules (dir, langs) =
  match (fromGlob $ "posts" </> dir </> "*") $ do
    route postRoute
    compile $ do
      currentPath <- getResourceFilePath
      let lang = takeBaseName currentPath
      let i18nItems = I18n.emptyLanguageItems langs
      pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" (postCtx (Just (return i18nItems)))
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx lang)
        >>= relativizeUrls

feedRules :: (String, String) -> Rules ()
feedRules (lang, feedType) = do
  let fileName = lang </> feedType <.> "xml"
  let snapshotPath = fromGlob $ "posts" </> "*" </> lang <.> "md"
  let feedFunction = if feedType == "rss" then renderRss else renderAtom
  create [fromFilePath fileName] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots snapshotPath "content"
      let feedCtx =
            postCtx Nothing `mappend`
            bodyField "description"
      feedFunction feedConfiguration feedCtx posts

-- Compilers

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
    constField "postsHeader" (I18n.languageName lang) `mappend`
    constField "title" "Harfang's Perch" `mappend`
    paginateContext paginate pageNumber `mappend`
    defaultContext

postCtx :: Maybe (Compiler [ Item String ] ) -> Context String
postCtx mbI18nUrls =
  case mbI18nUrls of
    Just i18nUrls ->
      listField "i18nUrls" (i18nCtx I18n.postLinkUrl I18n.languageName) i18nUrls `mappend`
      dateField "date" "%F" `mappend`
      defaultContext
    Nothing ->
      dateField "date" "%F" `mappend`
      defaultContext

postsCtx :: String -> Context String
postsCtx lang =
    teaserField "teaser" "content" `mappend`
    dateField "date" "%F" `mappend`
    constField "readMoreLinkText" (I18n.readMoreLinkText lang) `mappend`
    defaultContext

defaultCtx :: String -> Context String
defaultCtx lang =
    listField "langs" (i18nCtx I18n.indexLinkUrl I18n.languageName) (return . I18n.emptyLanguageItems $ I18n.mainLangs) `mappend`
    constField "postsLinkText" (I18n.postsLinkText lang) `mappend`
    constField "postsLinkUrl" (I18n.postsLinkUrl lang) `mappend`
    constField "aboutLinkText" (I18n.aboutLinkText lang) `mappend`
    constField "aboutLinkUrl" (I18n.aboutLinkUrl lang) `mappend`
    constField "atomFeedUrl" (I18n.atomFeedUrl lang) `mappend`
    constField "rssFeedUrl" (I18n.rssFeedUrl lang) `mappend`
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
  customRoute (\identifier -> I18n.postLinkUrl . toFilePath $ identifier)

-- Helpers
listFilesPerPost :: IO [(FilePath, [FilePath])]
listFilesPerPost = do
  directories <- listDirectory "./posts"
  mapM (\dir -> (fmap (((,) dir) . (map (\fileName -> "posts" </> dir </> fileName)))) (listDirectory $ "posts" </> dir)) directories

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- Paginate
postsPageId :: String -> PageNumber -> Identifier
postsPageId lang n = fromFilePath $ if (n == 1) then lang </> "index.html" else lang </> show n </> "index.html"

postsGrouper :: MonadMetadata m => [Identifier] -> m [[Identifier]]
postsGrouper = liftM (paginateEvery 10) . sortRecentFirst

postsPattern :: String -> Pattern
postsPattern lang =
  (fromGlob ("posts" </> "*" </> lang <.> "md"))

-- RSS/Atom Feed
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Harfang's Perch"
    , feedDescription = "On software in general, mostly functional"
    , feedAuthorName  = "harfangk"
    , feedAuthorEmail = ""
    , feedRoot        = "https://harfangk.page"
    }
