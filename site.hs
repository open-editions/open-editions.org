--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
import Data.Monoid (mappend)
import Prelude hiding ( div, span )
import Control.Monad (forM_)
import Hakyll
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty      ( renderHtml )
import Data.List (sortBy,isSuffixOf)
import System.FilePath.Posix (takeBaseName,takeDirectory,(</>))
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/**/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.hs" $ do
      route   $ setExtension "css"
      compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= applyTemplate defaultTemplate defaultContext
            >>= relativizeUrls

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    match "posts/*" $ do
        route niceRoute
        compile $ do pandocCompiler
          >>= applyTemplate postTemplate (postCtxWithTags tags)
          >>= saveSnapshot "content" -- for RSS
          >>= applyTemplate defaultTemplate defaultContext
          >>= relativizeUrls
          >>= cleanIndexUrls

    -- TODO: Tags page listing all tags
    create ["tags.html"] $ do
        route idRoute
        let tagList = tagsMap tags
        compile $ do
            makeItem ""
              >>= applyTemplate tagListTemplate (defaultCtxWithTags tags)

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) `mappend`
                    constField "title" "Posts"                `mappend`
                    defaultContext
            makeItem ""
                >>= applyTemplate postListTemplate indexCtx
                >>= applyTemplate defaultTemplate defaultContext
                >>= relativizeUrls
                >>= cleanIndexUrls

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description" 
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderAtom myFeedConfiguration feedCtx posts

    create ["feed.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description" 
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts

    tagsRules tags $ \tag pat -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let postCtx = postCtxWithTags tags
                postsField = listField "posts" postCtx (pure posts)
                titleField = constField "title" ("Posts tagged \""++tag++"\"")
                indexCtx = postsField <> titleField <> defaultContext
            makeItem "" >>= applyTemplate postListTemplate indexCtx
                        >>= applyTemplate defaultTemplate defaultContext
                        >>= relativizeUrls
                        >>= cleanIndexUrls
--------------------------------------------------------------------------------

-- Add tags to post context.
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

-- Add tags to default context, for tag listing
defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags         `mappend`
                          defaultContext
  where getAllTags :: Compiler [Item (String, [Identifier])]
        getAllTags = mapM (pure . mkItem) $ tagsMap tags
          where mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
                mkItem x@(t, _) = Item (tagsMakeId tags t) x
        tagsCtx :: Context (String, [Identifier])
        tagsCtx = listFieldWith "posts" postsCtx getPosts             `mappend`
                  metadataField                                       `mappend`
                  urlField "url"                                      `mappend`
                  pathField "path"                                    `mappend`
                  titleField "title"                                  `mappend`
                  missingField
          where getPosts :: Item (String, [Identifier])
                         -> Compiler [Item String]
                getPosts (itemBody -> (_, is)) = mapM load is
                postsCtx :: Context String
                postsCtx = postCtxWithTags tags

-- This creates permalinks in the form /YYYY/MM/permalink-to-blog-post/index.html
-- Assuming that post filenames are in the form YYYY-MM-permalink-to-blog-post.md
niceRoute :: Routes
niceRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p
                                 </> year
                                 </> month
                                 </> drop 11 filename
                                 </> "index.html"
                           where
                             year = take 4 filename
                             month = take 2 $ drop 5 $ filename
                             filename = takeBaseName p
                             p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
    where
        idx = "index.html"
        clean url
            | idx `isSuffixOf` url = take (length url - length idx) url
            | otherwise            = url

postCtx :: Context String
postCtx =
    dateField "date" "%0Y-%m-%d" `mappend`
    defaultContext

-- Atom / RSS Config
myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Jonathan Reeve: Computational Literary Analysis"
    , feedDescription = "Personal website of Jonathan Reeve, with blog posts about computational literary analysis."
    , feedAuthorName  = "Jonathan Reeve"
    , feedAuthorEmail = "jon.reeve@gmail.com"
    , feedRoot        = "http://jonreeve.com"
    }

-- Templates
defaultTemplate :: Template
defaultTemplate = readTemplate . renderHtml $ defaultTemplateRaw

defaultTemplateRaw :: Html
defaultTemplateRaw = html $ do
    H.head $ do
        meta ! httpEquiv "Content-Type" ! content "text/html; charset=UTF-8"
        H.title "Jonathan Reeve: $title$"
        mapM_ ((link ! rel "stylesheet" ! type_ "text/css" !) . href)
          ["https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
         , "/assets/css/vendor/skeleton.css"
         , "/css/style.css"
         ]
    body $ do
        H.div ! A.class_ "container-fluid" $ do
          H.nav ! A.class_ "navbar navbar-expand-lg navbar-dark stylish-color" $ do
              H.a ! A.class_ "navbar-brand" ! href "/" $ "Jonathan Reeve: computational literary analysis"
              H.ul ! A.class_ "nav navbar-nav ml-auto" $ do
                  a ! A.class_ "nav-link active" ! href "/" $ "Posts"
                  a ! A.class_ "nav-link" ! href "/cv" $ "CV"
                  a ! A.class_ "nav-link" ! href "/tags" $ "Tags"
          H.div ! A.id "content" ! A.class_ "container" $ do
              h1 "$title$"
              "$body$"
        H.footer ! A.class_ "page-footer stylish-color-dark container-fluid" $ do
          H.div ! A.class_ "row container-fluid" $ do
            H.div ! A.class_ "col" $ do
              "This work is licensed under a "
              a ! href "http://creativecommons.org/licenses/by-nc-sa/4.0/" $
                "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License."
              "All content © Jonathan Reeve 2018. Created using free and open-source software." 
              "Site proudly generated by "
              a ! href "http://jaspervdj.be/hakyll" $ "Hakyll"
            H.div ! A.class_ "col" ! A.id "social" $ do
              a ! href "http://twitter.com/j0_0n" $ i ! A.class_ "fa fa-twitter" $ mempty
              a ! href "http://github.com/JonathanReeve" $ i ! A.class_ "fa fa-github" $ mempty
              a ! href "http://instagram.com/jonreeve" $ i ! A.class_ "fa fa-instagram" $ mempty
              a ! href "mailto:jon.reeve@gmail.com" $ i ! A.class_ "fa fa-envelope" $ mempty
    H.script ! src "https://code.jquery.com/jquery-3.2.1.slim.min.js" $ mempty
    H.script ! src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js" $ mempty
    H.script ! src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" $ mempty

postListTemplate :: Template
postListTemplate = readTemplate . renderHtml $ postListTemplateRaw

postListTemplateRaw :: Html
postListTemplateRaw =
  ul $ do
    "$for(posts)$"
    li ! A.class_ "" $ do
        "$date$: "
        a ! href "$url$" $ "$title$"
        p ! class_ "taglist" $ "$if(tags)$ Tags: $tags$ $endif$"
    "$endfor$"

postTemplate :: Template
postTemplate = readTemplate . renderHtml $ postTemplateRaw

postTemplateRaw :: Html
postTemplateRaw = H.section ! class_ "container" $ do
  div ! class_ "container" $ do
    section ! class_ "info" $ do
      "Posted on $date$\n"
      "$if(tags)$"
      "Tags: $tags$"
      "$endif$"
    section ! class_ "postBody" $ do
      "$body$"

tagListTemplate = readTemplate . renderHtml $ tagListTemplateRaw

tagListTemplateRaw :: Html
tagListTemplateRaw =
  ul $ do
    "$for(tags)$"
    li ! A.class_ "" $ do
        a ! href "$url$" $ "$title$"
        ul $ do
          "$for(posts)$"
          li ! A.class_ "" $ do
            a ! href "$url$" $ "$title$"
          "$endfor$"
    "$endfor$"
