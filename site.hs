--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Data.List (isSuffixOf)

--------------------------------------------------------------------------------

siteHost :: String
siteHost = "http:://matrixwood.netlify.com"

siteName :: String
siteName = "matrixwood"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
          { destinationDirectory = "public"
          , previewHost          = "0.0.0.0"
          , previewPort          = 4000
          , deployCommand        = "stack exec page build; echo \"\n\n *** add rsync command ***\""
          }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = siteName
    , feedDescription = "RSS feed for matrixwood."
    , feedAuthorName = "wwc7033@gmail.com CanftIn"
    , feedAuthorEmail = "wwc7033@gmail.com"
    , feedRoot = "http:://matrixwood.netlify.com"
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "downloads/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["projects.md", "about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["writing.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let writingCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Writing"             <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/writing.html" writingCtx
                >>= loadAndApplyTemplate "templates/default.html" writingCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
              loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts
              >>= cleanIndexHtmls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (recentFirst =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return (take 5 posts)) <>
                    constField "title" "Home"                         <>
                    defaultContext

            getResourceBody
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= relativizeUrls




    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    teaserField "teaser" "content" <>
    defaultContext


cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"
    replacement = const "/"
