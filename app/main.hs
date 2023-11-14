--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

--------------------------------------------------------------------------------
conf :: Configuration
conf =
  defaultConfiguration
    { providerDirectory = "src"
    , destinationDirectory = "dist"
    , previewHost = "0.0.0.0"
    , previewPort = 8080
    }

--------------------------------------------------------------------------------
postsPattern :: Pattern
postsPattern = "posts/**"

ctx :: Context String
ctx = constField "siteTitle" "Bibliotheca ex Machina" `mappend` defaultContext

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith conf $ do
  match postsPattern $ do
    route $ setExtension "html"
    compile
      $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" postCtx
      >>= loadAndApplyTemplate "templates/base.html" postCtx
  -- >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let archiveCtx = listField "posts" postCtx (return posts) `mappend` ctx

      getResourceBody
        >>= applyAsTemplate archiveCtx
        -- >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/base.html" archiveCtx
  -- >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll postsPattern
      let recentPosts = take 5 posts
          indexCtx = listField "posts" postCtx (return recentPosts) `mappend` ctx

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/base.html" indexCtx
  -- >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "**" $ do
    route idRoute
    compile copyFileCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = ctx

-- dateField "date" "%B %e, %Y"
--  `mappend` defaultContext
