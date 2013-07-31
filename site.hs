--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import Control.Monad.IO.Class


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  let copyRule = route idRoute >> compile copyFileCompiler
  match ".travis.yml"    copyRule
  match "CNAME"          copyRule
  match "images/*"       copyRule
  match "cv/*"           copyRule
  match "cheat-sheets/*" copyRule

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
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
            field "posts" (\_ -> postList recentFirst) `mappend`
            constField "title" "Archives"              `mappend`
            defaultContext

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
        >>= (\a -> debugCompiler (show a) >> return a)
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
  posts   <- sortFilter =<< loadAll "posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  applyTemplateList itemTpl postCtx posts
