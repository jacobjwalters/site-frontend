-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import           Data.Text hiding (take, stripPrefix)
import           Text.Regex
import           System.FilePath (replaceExtension)

import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers (writePlain)

import           Hakyll

--VARIABLES---------------------------------------------------------------------
-- | Root URL for the site
root :: String
root = "https://jacobwalte.rs"

--MAIN--------------------------------------------------------------------------
pandocWith :: (Pandoc -> Pandoc) -> Compiler (Item String)
pandocWith =
  pandocCompilerWithTransform
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions
  
pdc :: Compiler (Item String)
pdc = fmap demoteHeaders <$> pandocWith
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

-- | Remove everything apart from code blocks
extractCodeBlocks :: Pandoc -> Pandoc
extractCodeBlocks = walk $ \block -> case block of
  CodeBlock _ text -> Plain [Str text]
  _ -> Null

-- | Plaintext form of stripped code blocks
codeCompiler :: Compiler (Item String)
codeCompiler = do
  i <- getResourceBody
  d <- readPandocWith defaultHakyllReaderOptions i
  writePandoc <$> (traverse (pure . extractCodeBlocks) d)
  where writePandoc (Item itemi doc) =
          case runPure $ writePlain defaultHakyllWriterOptions doc of
            Left  err   -> error $ "Hakyll.Web.Pandoc.writePandocWith: " ++ show err
            Right item' -> Item itemi $ unpack item'

-- | Sets the route to the `permalink` tag, with the given extension.
permalinkExt :: String -> Routes
permalinkExt ext = metadataRoute $ \m ->
  case lookupString "permalink" m of
    Just pl -> constRoute $ replaceExtension pl ext
    Nothing -> setExtension ext

contentRoute :: Routes
contentRoute = customRoute ((fromMaybe <*> (stripPrefix "content/")) . toFilePath)

htmlc :: Compiler (Item String)
htmlc = pdc
    >>= loadAndApplyTemplate "templates/default.html" rootCtx
    >>= relativizeUrls

orgc :: Compiler (Item String)
orgc = getResourceBody
   >>= loadAndApplyTemplate "templates/default.org" defaultContext
   >>= relativizeUrls

htmlContent :: Rules ()
htmlContent = version "html" $ do
  route   (composeRoutes contentRoute (permalinkExt "html"))
  compile htmlc

orgContent :: Rules ()
orgContent = version "org" $ do
  route   (composeRoutes contentRoute (permalinkExt "org"))
  compile orgc

rawContent :: Rules ()
rawContent = do
  route   contentRoute
  compile copyFileCompiler

data FeedType = RSS | Atom

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
  { feedTitle       = "Jacob Walters' Blog"
  , feedDescription = "This feed serves the posts from my blog."
  , feedAuthorName  = "Jacob Walters"
  , feedAuthorEmail = "blog@this.domain"
  , feedRoot        = "https://jacobwalte.rs"
  }

feed :: FeedType -> Rules ()
feed ft = do
  route idRoute
  compile $ do
      posts <- recentFirst =<< loadAllSnapshots ("content/posts/*" .&&. hasVersion "html") "htmlContent"

      rssT      <- loadBody "templates/feeds/rss.xml"
      rssItemT  <- loadBody "templates/feeds/rss-item.xml"
      atomT     <- loadBody "templates/feeds/atom.xml"
      atomItemT <- loadBody "templates/feeds/atom-item.xml"
      let compiler = case ft of
                       RSS  -> renderRssWithTemplates  rssT  rssItemT
                       Atom -> renderAtomWithTemplates atomT atomItemT
      compiler feedConfig postCtx posts


main :: IO ()
main = hakyll $ do
    -- Static pages
    match (fromList ["content/about.org", "content/contact.md",  "content/links.org", "content/pl-glossary.org"]) htmlContent
    match (fromList ["content/about.org", "content/contact.org", "content/links.org", "content/pl-glossary.org"]) orgContent

    match (fromList ["content/robots.txt", "content/404.html"]) rawContent
    match "content/static/images/**" rawContent

    -- CSS
    match "content/static/style.org" $ version "css" $ do
        route   $ constRoute "static/style.css"
        compile $ fmap compressCss <$> codeCompiler

    match "content/static/style.org" htmlContent
    match "content/static/style.org" orgContent

    -- JS
    match "content/static/js.org" $ version "css" $ do
        route   $ constRoute "static/site.js"
        compile $ codeCompiler

    match "content/static/js.org" htmlContent
    match "content/static/js.org" orgContent

    -- Posts
    match "content/posts/*" $ version "html" $ do
        --route $ setExtension "html"
        route   $ composeRoutes contentRoute (permalinkExt "html")
        compile $ pdc
              >>= loadAndApplyTemplate "templates/post.html"    postCtx
              >>= saveSnapshot "htmlContent"
              >>= loadAndApplyTemplate "templates/default.html" postCtx
              >>= relativizeUrls

    match "content/posts/*" orgContent


    -- Feeds
    create ["atom.xml"] $ feed Atom
    create ["rss.xml"]  $ feed RSS

    -- Post archives
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("content/posts/*" .&&. hasVersion "html")
            let archiveCtx =
                    listField  "posts" postCtx (pure posts) <>
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
            posts <- recentFirst =<< loadAll ("content/posts/*" .&&. hasVersion "org")
            let archiveCtx =
                    listField "posts" postCtx (pure posts) <>
                    constField "title"       "Archives"      <>
                    constField "description" "Post archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.org" archiveCtx
                >>= loadAndApplyTemplate "templates/default.org" archiveCtx
                >>= relativizeUrls


    -- Sitemap
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("content/posts/*" .&&. hasNoVersion)
            singlePages <- loadAll (fromList ["content/about.org", "content/contact.org", "content/links.org", "archive.html"] .&&. hasNoVersion)

            let sitemapCtx =
                    constField "root" root <>
                    listField "singlepages" rootCtx (pure singlePages) <>
                    listField "posts"       postCtx (pure posts)

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


    -- Homepages
    match "content/index.html" $ do
        route contentRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("content/posts/*" .&&. hasVersion "html")
            let indexCtx =
                    listField "posts" postCtx (pure $ take 5 posts) <>
                    rootCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/homepage.html" indexCtx
                >>= relativizeUrls

    match "content/index.org" $ do
        route contentRoute
        compile $ do
            posts <- recentFirst =<< loadAll ("content/posts/*" .&&. hasVersion "org")
            let indexCtx =
                    listField "posts" postCtx (pure $ take 5 posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.org" indexCtx
                >>= relativizeUrls


    -- Templates
    match "templates/**" $ compile templateBodyCompiler


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

