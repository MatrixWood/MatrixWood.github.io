--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll

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
