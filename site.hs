--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Text
import           Text.Regex

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Pandoc.SideNote (usingSideNotes)

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
  (convertOrgLinks . removeFootnotesHeader . usingSideNotes)

-- | Convert links from .org files to .html
convertOrgLinks :: Pandoc -> Pandoc
convertOrgLinks = walk $ \inline -> case inline of
    Link attr il (url, title) -> Link attr il (pack (orgRegex (unpack url)), title)
    Span attr@("", ["spurious-link"], [("target", tgt)]) il -> Link attr il (internalLink tgt, tgt)
    _ -> inline
  where
    orgRegex str = subRegex (mkRegex "^(.*?)\\.org$") str "\\1.html"
    internalLink = cons '#' . Data.Text.intercalate "-" . splitOn " " . toLower
    

-- | Remove empty footnotes header from the bottom of the page
removeFootnotesHeader :: Pandoc -> Pandoc
removeFootnotesHeader = walk $ \inline -> case inline of
    Header 1 ("footnotes", [], []) _ -> Null
    _ -> inline

main :: IO ()
main = hakyll $ do
    match (fromList ["images/*", "robots.txt", "404.html"]) $ do
        route   idRoute
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
