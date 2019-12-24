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

-- | Make the nav area.
navArea ::
  Html () ->  -- The main brand / name of the site.
  [ (Text, Html () ) ] -- A list of nav items (url, displayName)
  -> Html () -- The Lucid Html for the nav area.
navArea brand navItems =
  nav_ [ class_ "light-blue lighten-1", role_ "navigation" ] $ div_ [ class_ "nav-wrapper container" ] $ do
    a_ [ id_ "logo-container", href_ "#", class_ "brand-logo" ] $ brand
    ul_ [ class_ "right hide-on-med-and-down" ] $ forM_ navItems $ \navItem ->
      li_ $ a_ [ href_ (fst navItem) ] (snd navItem)
    ul_ [ id_ "nav-mobile", class_ "sidenav" ] $ forM_ navItems $ \navItem ->
      li_ $ a_ [ href_ (fst navItem) ] (snd navItem)
    a_ [ href_ "#", Lucid.data_ "target" "nav-mobile", class_ "sidenav-trigger" ] $ i_ [ class_ "material-icons" ] $ "menu"

-- | Make the three big cards that appear with icons on the main page.
iconBlock :: (Html (), Html (), Html ()) -> Html ()
iconBlock (icon, blurb, description) =
  div_ [ class_ "col s12 m4" ] $ div_ [ class_ "icon-block" ] $ do
    h2_ [ class_ "center light-blue-text" ] $ i_ [ class_ "material-icons" ] $ icon
    h5_ [ class_ "center" ] $ blurb
    p_ [ class_ "light" ] $ description

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
      docs <- Rib.buildHtmlMulti patterns $ renderPage . Page_Doc

      -- Build an index.html linking to the aforementioned files.
      Rib.buildHtml [relfile|index.html|] $ renderPage $ Page_Index docs

    -- File patterns to build, using the associated markup parser
    patterns = Map.fromList [ ([relfile|*.md|], Some Rib.Markup_MMark)]

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
        link_ [rel_ "stylesheet", href_ "static/css/materialize.min.css"]
        link_ [ href_ "https://fonts.googleapis.com/icon?family=Material+Icons", rel_ "stylesheet" ]
      body_
        $ div_ [class_ "container"]
        $ do
          navArea "Open Editions" [ ("about/", "About"),
                                    ("contribute/", "Contribute"),
                                    ("texts/", "Texts")
                                  ]
          case page of
            Page_Index docs -> do
              div_ [ class_ "section no-pad-bot", id_ "index-banner" ] $ div_ [ class_ "container" ] $ do
                br_ []
                h1_ [ class_ "header center orange-text" ] $ "Open Editions"
                div_ [ class_ "row center" ] $ h5_ [ class_ "header col s12 light" ] $ do
                  "Open-Source Electronic Scholarly Editions "
                  br_ []
                  "of Public Domain Literature"
                div_ [ class_ "row center" ] $ a_ [ href_ "about.html", id_ "download-button", class_ "btn-large waves-effect waves-light orange" ] $ "Learn More"
                br_ []
              div_ [ class_ "container" ] $ do
                div_ [ class_ "section" ] $ do
                  div_ [ class_ "row" ] $ do
                    iconBlock ("flash_on", "Standards-Focused", "We did most of the heavy lifting for you to provide a default stylings that incorporate our custom components. Additionally, we refined animations and transitions to provide a smoother experience for developers.") 
                    iconBlock ("group", "User Experience Focused", "By utilizing elements and principles of Material Design, we were able to create a framework that incorporates components and animations that provide more feedback to users. Additionally, a single underlying responsive system across all platforms allow for a more unified user experience.")
                    iconBlock ("settings", "Easy to work with", "We have provided detailed documentation as well as specific code examples to help new users get started. We are also always open to feedback and can answer any questions a user may have about Materialize.") 
              div_ $ forM_ docs $ \doc -> with li_ [class_ "links"] $ do
                let meta = Rib.documentMeta doc
                b_ $ with a_ [href_ (Rib.documentUrl doc)] $ toHtml $ title meta
                maybe mempty Rib.renderMarkdown $ description meta
            Page_Doc doc ->
              article_ [class_ "post container"] $ do
                h1_ $ toHtml $ title $ Rib.documentMeta doc
                Rib.documentHtml doc

          footer_ [ class_ "page-footer orange" ] $ do
            div_ [ class_ "container" ] $ div_ [ class_ "row" ] $ do
              div_ [ class_ "col l6 s12" ] $ do
                h5_ [ class_ "white-text" ] $ "Open Editions"
                p_ [ class_ "grey-text text-lighten-4" ] $ "We are volunteers doing this in our spare time, just for the love of it. Any help would be much appreciated!" 
              div_ [ class_ "col l4 s12" ] $ do
                h5_ [ class_ "white-text" ] $ "Texts"
                ul_ $ do
                  li_ $ a_ [ class_ "white-text", href_ "texts/Ulysses/" ] $ "Ulysses"
                  li_ $ a_ [ class_ "white-text", href_ "texts/Portrait/" ] $ "Portrait"
                  li_ $ a_ [ class_ "white-text", href_ "texts/Dubliners/" ] $ "Dubliners"
                  li_ $ a_ [ class_ "white-text", href_ "texts/Middlemarch/" ] $ "Middlemarch"
            div_ [ class_ "footer-copyright" ] $ div_ [ class_ "container" ] $ do
              "Made with "
              a_ [ class_ "orange-text text-lighten-3", href_ "http://materializecss.com" ] $ "Materialize, "
              a_ [ class_ "orange-text text-lighten-3", href_ "https://github.com/srid/rib" ] $ "Rib, "
              a_ [ class_ "orange-text text-lighten-3", href_ "http://haskell.org" ] $ "and Haskell."

          -- script_ [ src_ "https://code.jquery.com/jquery-2.1.1.min.js" ] ""
          -- script_ [ src_ "js/materialize.js" ] ""
          -- script_ [ src_ "js/init.js" ] ""

    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div#thesite" ? do
      margin (em 4) (pc 20) (em 1) (pc 20)
      "li.links" ? do
        listStyleType none
        marginTop $ em 1
        "b" ? fontSize (em 1.2)
        "p" ? sym margin (px 0)
