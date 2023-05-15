--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Text
import           Text.Regex

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Pandoc.SideNote (usingSideNotes)

import Debug.Trace

import           Hakyll


--------------------------------------------------------------------------------
-- | Root URL for the site
root :: String
root = "https://jacobwalte.rs"

--------------------------------------------------------------------------------
pdc :: Compiler (Item String)
pdc = pandocCompilerWithTransform
  defaultHakyllReaderOptions
  defaultHakyllWriterOptions
  (convertOrgLinks . usingSideNotes)

-- | Convert links from .org files to .html
convertOrgLinks :: Pandoc -> Pandoc
convertOrgLinks = walk $ \inline -> case inline of
    Link attr inline (url, title) -> Link attr inline (pack (orgRegex (unpack url)), title)
    _ -> inline
  where
    orgRegex :: String -> String
    orgRegex str = subRegex (mkRegex "^(.*?)\\.org$") str "\\1.html"

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.org", "contact.md", "links.org"]) $ do
        route   $ setExtension "html"
        compile $ pdc
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match (fromList ["about.org", "contact.org", "links.org"]) $ version "org" $ do
        route   $ setExtension "org"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.org" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pdc
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ version "org" $ do
        route idRoute
        compile $ getResourceBody
          >>= loadAndApplyTemplate "templates/default.org"    postCtx
          >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title"       "Archives"      <>
                    constField "description" "Post archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["archive.org"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "org")
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title"       "Archives"      <>
                    constField "description" "Post archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.org" archiveCtx
                >>= loadAndApplyTemplate "templates/default.org" archiveCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            singlePages <- loadAll (fromList ["about.org", "contact.org", "links.org", "archive.org"] .&&. hasNoVersion)
            let sitemapCtx =
                    constField "root" root <>
                    listField "singlepages" rootCtx (pure singlePages) <>
                    listField "posts"       postCtx (pure posts)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasNoVersion)
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/homepage.html" indexCtx
                >>= relativizeUrls

    match "index.org" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("posts/*" .&&. hasVersion "org")
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.org" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
-- | Extend the default context with the root of the site
rootCtx :: Context String
rootCtx = constField "root" root <> defaultContext

postCtx :: Context String
postCtx =
    constField "root" root <>
    dateField "isodate" "%Y-%m-%d" <>
    dateField "date" "%B %e, %Y" <>
    rootCtx
