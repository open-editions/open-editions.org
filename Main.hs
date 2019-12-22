{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Clay hiding (title, type_)
import Control.Monad
import Data.Aeson (FromJSON)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Some (Some (..))
import Data.Text (Text)
import Development.Shake
import GHC.Generics
import Lucid
import Path
import Rib (Document)
import qualified Rib

-- First we shall define two datatypes to represent our pages. One, the page
-- itself. Second, the metadata associated with each document.

-- | A generated page is either an index of documents, or an individual document.
--
-- The `Document` type takes two type variables:
-- 1. The first type variable specifies the parser to use: MMark or Pandoc
-- 2. The second type variable should be your metadata record
data Page
  = Page_Index [Document DocMeta]
  | Page_Doc (Document DocMeta)

-- | Type representing the metadata in our Markdown documents
--
-- Note that if a field is not optional (i.e., not Maybe) it must be present.
data DocMeta
  = DocMeta
      { title :: Text,
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Metadata for the site itself.
data Site
  = Site
      { siteTitle :: Text,
        authorName :: Text,
        authorEmail :: Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `a`, from which static files will be read.
-- 2. Directory `b`, under which target files will be generated.
-- 3. Shake build action to run.
--
-- In the shake build action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|src|] [reldir|dist|] generateSite
  where
    -- Shake Action for generating the static site
    generateSite :: Action ()
    generateSite = do
      -- Copy over the static files
      Rib.buildStaticFiles [[relfile|static/**|]]
      -- Build individual markup sources, generating .html for each.
      docs <-
        Rib.buildHtmlMulti patterns $
          renderPage . Page_Doc
      -- Build an index.html linking to the aforementioned files.
      Rib.buildHtml [relfile|index.html|]
        $ renderPage
        $ Page_Index docs
    -- File patterns to build, using the associated markup parser
    patterns =
      Map.fromList
        [ ([relfile|*.md|], Some Rib.Markup_MMark)
        ]
    -- Define your site HTML here
    renderPage :: Page -> Html ()
    renderPage page = with html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        title_ $ case page of
          Page_Index _ -> "Open Editions"
          Page_Doc doc -> toHtml $ title $ Rib.documentMeta doc
        -- style_ [type_ "text/css"] $ Clay.render pageStyle
        link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"]
        link_ [ href_ "https://fonts.googleapis.com/icon?family=Material+Icons", rel_ "stylesheet" ]
      body_
        $ div_ [class_ "container"]
        $ do
          nav_ [ class_ "light-blue lighten-1", role_ "navigation" ] $ div_ [ class_ "nav-wrapper container" ] $ do
            a_ [ id_ "logo-container", href_ "#", class_ "brand-logo" ] $ "Open Editions"
            ul_ [ class_ "right hide-on-med-and-down" ] $ do
              li_ $ a_ [ href_ "about/" ] "About"
              li_ $ a_ [ href_ "contribute/" ] "Contribute"
            ul_ [ id_ "nav-mobile", class_ "sidenav" ] $ do
              li_ $ a_ [ href_ "#" ] "About"
              li_ $ a_ [ href_ "#" ] "Contribute"
            a_ [ href_ "#", Lucid.data_ "target" "nav-mobile", class_ "sidenav-trigger" ] $ i_ [ class_ "material-icons" ] $ "menu"
          case page of
            Page_Index docs -> do
              div_ [ class_ "section no-pad-bot", id_ "index-banner" ] $ div_ [ class_ "container" ] $ do
                br_ []
                h1_ [ class_ "header center orange-text" ] $ "Open Editions"
                div_ [ class_ "row center" ] $ h5_ [ class_ "header col s12 light" ] $ "Open-Source Electronic Scholarly Editions of Public Domain Literature"
                div_ [ class_ "row center" ] $ a_ [ href_ "about.html", id_ "download-button", class_ "btn-large waves-effect waves-light orange" ] $ "Learn More"
                br_ []
              div_ $ forM_ docs $ \doc -> with li_ [class_ "links"] $ do
                let meta = Rib.documentMeta doc
                b_ $ with a_ [href_ (Rib.documentUrl doc)] $ toHtml $ title meta
                maybe mempty Rib.renderMarkdown $ description meta
            Page_Doc doc ->
              with article_ [class_ "post"] $ do
                h1_ $ toHtml $ title $ Rib.documentMeta doc
                Rib.documentHtml doc
    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div#thesite" ? do
      margin (em 4) (pc 20) (em 1) (pc 20)
      "li.links" ? do
        listStyleType none
        marginTop $ em 1
        "b" ? fontSize (em 1.2)
        "p" ? sym margin (px 0)
