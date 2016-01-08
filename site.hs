{-# LANGUAGE OverloadedStrings #-}

import Hakyll
import System.FilePath.Posix (replaceExtension, takeFileName, takeBaseName)
import Data.Monoid ((<>))

main :: IO ()
main = hakyll $ do
  let copyRule = route idRoute >> compile copyFileCompiler
  match "CNAME"    copyRule
  match "images/*" copyRule
  match "cv/*"     copyRule

  match "redirects/*" $ do
    route $ customRoute $ flip replaceExtension "html" . takeFileName . toFilePath
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/redirect.html" siteCtx

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["projects.markdown", "about.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls

  match "posts/*" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  {-
  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList recentFirst) <>
            constField "title" "Archives"              <>
            siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls
  -}

  match "index.html" $ do
    route idRoute
    compile $ do
      let indexCtx = field "posts" $ \_ -> postList (fmap (take 3) . recentFirst)
      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  siteCtx

siteCtx :: Context String
siteCtx = 
  activeClassField <>
  defaultContext

-- https://groups.google.com/forum/#!searchin/hakyll/if$20class/hakyll/WGDYRa3Xg-w/nMJZ4KT8OZUJ 
activeClassField :: Context a 
activeClassField = functionField "activeClass" $ \[p] _ -> do 
  path <- takeBaseName . toFilePath <$> getUnderlying
  return $ if path == p then "active" else path 

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts
