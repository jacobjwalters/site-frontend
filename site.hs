-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Text
import           Text.Regex
import           System.FilePath (replaceExtension)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk

import           Hakyll

--VARIABLES---------------------------------------------------------------------
-- | Root URL for the site
root :: String
root = "https://jacobwalte.rs"

--MAIN--------------------------------------------------------------------------
pdc :: Compiler (Item String)
pdc = fmap demoteHeaders <$> pandocCompilerWithTransform
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        ( convertOrgLinks
        . removeFootnotesHeader
        )

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

permalinkExt :: String -> Routes
permalinkExt ext = metadataRoute $ \m ->
  case lookupString "permalink" m of
    Just pl -> constRoute $ replaceExtension pl ext
    Nothing -> setExtension ext

main :: IO ()
main = hakyll $ do
    match (fromList ["robots.txt", "404.html"]) $ do
        route   idRoute
        compile copyFileCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["about.org", "contact.md", "links.org"]) $ do
        route   $ setExtension "html"
        compile $ pdc
            >>= loadAndApplyTemplate "templates/default.html" rootCtx
            >>= relativizeUrls

    match (fromList ["about.org", "contact.org", "links.org"]) $ version "org" $ do
        route   $ setExtension "org"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.org" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        --route $ setExtension "html"
        route $ permalinkExt "html"
        compile $ pdc
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ version "org" $ do
        route $ setExtension "org"
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
                    constField "description" "The complete list of all posts available on this site." <>
                    rootCtx

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
            singlePages <- loadAll (fromList ["about.org", "contact.org", "links.org", "archive.html"] .&&. hasNoVersion)
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
                    rootCtx

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


--CONTEXTS----------------------------------------------------------------------
-- | Extend the default context with the root of the site
rootCtx :: Context String
rootCtx =
    constField "root" root <>
    pathNoExt <>
    defaultContext

postCtx :: Context String
postCtx =
    dateField "isodate" "%Y-%m-%d" <>
    dateField "date" "%B %e, %Y" <>
    rootCtx

pathNoExt :: Context a
pathNoExt = field "pathNoExt" $ pure . Prelude.takeWhile (/= '.') . toFilePath . itemIdentifier

