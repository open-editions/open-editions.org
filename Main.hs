{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Clay hiding (id, title, type_)
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import GHC.Generics
import Lucid
import Path
import Rib (MMark, IsRoute)
import qualified Rib
import qualified Rib.Parser.MMark as MMark

import qualified Editions as E -- my own module

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

-- Route for each generated static page. 
data Route a where
  Route_Index :: Route ()
  Route_Doc :: Path Rel File -> Route MMark
  Route_Texts :: Route ()

-- Define the target .html path for each route here.
instance IsRoute Route where 
  routeFile = \case
    Route_Index ->
      pure [relfile|index.html|]
    Route_Doc srcPath -> do 
      dir <- parseRelDir =<< fmap (toFilePath . fst) (splitExtension srcPath)
      pure $ dir </> [relfile|index.html|]
    Route_Texts -> do
      pure $ [relfile|texts/index.html|]

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

getMeta :: MMark -> DocMeta
getMeta src = case MMark.projectYaml src of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v

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

      let writeHtmlRoute :: Route a -> a -> Action ()
          writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderRoute r
   
      -- Build individual markup sources, generating .html for each.
      Rib.forEvery [[relfile|**/*.md|]] $ \srcPath -> do
        let r = Route_Doc srcPath
        doc <- MMark.parse srcPath
        writeHtmlRoute r doc
      
      -- Build an index.html linking to the aforementioned files.
      writeHtmlRoute Route_Index ()

      -- Build a texts directory using the data in Editions.hs.
      writeHtmlRoute Route_Texts ()

    -- Define your site HTML here
    renderRoute :: Route a -> a -> Html ()
    renderRoute route val = html_ [lang_ "en"] $ do
      head_ $ do
        meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
        title_ $ toHtml @Text $ case route of
          Route_Index -> "Open Editions"
          Route_Doc _ -> title $ getMeta val
          Route_Texts -> "Open Editions Texts"
        style_ [type_ "text/css"] $ Clay.render pageStyle
        link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"]
        link_ [rel_ "stylesheet", href_ "/static/css/materialize.min.css"]
        link_ [ href_ "https://fonts.googleapis.com/icon?family=Material+Icons", rel_ "stylesheet" ]

      body_ $ do
        navArea "Open Editions" [ ("/about/", "About")
                                      , ("/contribute/", "Contribute")
                                      , ("/texts/", "Texts")
                                      ]
        div_ [class_ "container"] $
          renderRouteBody val route

        footerTemplate
        scriptsTemplate

    renderRouteBody :: a -> Route a -> Html ()
    renderRouteBody val = \case
      Route_Index ->
        indexTemplate
      Route_Texts -> textsTemplate
      Route_Doc _ -> do
        let meta = getMeta val
        article_ [class_ "post container"] $ do
          h1_ $ toHtml $ title meta
          MMark.render val

    -- Define your site CSS here
    pageStyle :: Css
    pageStyle = "div.container" ? do
      "img" ? do
        maxWidth (pct 100)

textsTemplate :: Html ()
textsTemplate = do
  -- let meta = getMeta doc
  article_ [class_ "post container"] $ do
    -- h1_ $ toHtml $ title meta
    -- MMark.render $ Rib.sourceVal doc
    h1_ "Open Editions Texts"
    div_ [ class_ "section" ] $ do
      mapM_ formatEdition E.editions

textButton :: Text -> Html () -> Html () -> Html ()
textButton uri iconName buttonText =
  case uri of
    -- Disable the button if it points to an empty URL
    "" -> a_ [ class_ "btn-small disabled" ] $ do
           i_ [ class_ "material-icons left" ] iconName
           buttonText
    otherwise -> a_ [ class_ "btn-small", href_ uri ] $ do
                  i_ [ class_ "material-icons left" ] iconName
                  buttonText

formatEdition :: E.Edition -> Html ()
formatEdition ed = do
  h3_ [id_ (E.slug ed)] $ toHtml $ E.title ed
  h5_ $ toHtml $ E.author ed
  div_ [ class_ "info" ] $ markdownToHtml (E.provenance ed)
  div_ [ class_ "buttons" ] $ do
    let github = "https://github.com/open-editions/" :: Text
        repoUrl = T.concat [github, E.repo ed]
        issuesUrl = E.issues $ E.repo ed
        contributorsUrl = E.contributors $ E.repo ed
        previewUrl = E.preview ed
    textButton repoUrl "cloud" "GitHub Repo"
    textButton issuesUrl "done" "Issues"
    textButton contributorsUrl "people" "Contributors"
    textButton previewUrl "book" "Preview"

-- Convenience function, for transforming markdown strings to HTML
markdownToHtml :: E.Markdown -> Html ()
markdownToHtml md = do
  let result = MMark.parsePure "markdown" md
  case result of
      Left err -> error (T.unpack err)
      Right res -> MMark.render res

indexTemplate :: Html ()
indexTemplate =  do
  div_ [ class_ "section no-pad-bot", id_ "index-banner" ] $ div_ [ class_ "container" ] $ do
    br_ []
    h1_ [ class_ "header center orange-text" ] $ "Open Editions"
    div_ [ class_ "row center" ] $ h5_ [ class_ "header col s12 light" ] $ do
      "Open-Source Electronic Scholarly Editions "
      br_ []
      "of Public Domain Literature"
    div_ [ class_ "row center" ] $ a_ [ href_ "/about/", id_ "download-button", class_ "btn-large waves-effect waves-light orange" ] $ "Learn More"
    br_ []
  div_ [ class_ "container" ] $ do
    div_ [ class_ "section" ] $ do
      div_ [ class_ "row" ] $ do
        iconBlock ("settings", "Open", "This is a community-run project, made by a large network of literary scholars, librarians, students, and programmers from around the world. Anyone can get involved. All of our code and data is publicly available and remixable.")
        iconBlock ("group", "Scholarly", "We don't just want to create flashy book interfaces. We want to digitally represent books commensurately with the way we understand them. This means close attention to the text, and the history of its readings.")
        iconBlock ("flash_on", "Standards-Focused", "Our technology stack is meant to be modular, repeatable, and future-proof. This is not just another Digital Humanities project. We want to make the digital editions framework for the future.")
  -- div_ $ forM_ docs $ \doc -> with li_ [class_ "links"] $ do
  --   let meta = Rib.documentMeta doc
  --   b_ $ with a_ [href_ (Rib.documentUrl doc)] $ toHtml $ title meta
  --   maybe mempty Rib.renderMarkdown $ description meta

footerTemplate :: Html ()
footerTemplate = footer_ [ class_ "page-footer orange" ] $ do
            div_ [ class_ "container" ] $ div_ [ class_ "row" ] $ do
              div_ [ class_ "col l6 s12" ] $ do
                h5_ [ class_ "white-text" ] $ "Open Editions"
                p_ [ class_ "grey-text text-lighten-4" ] $ do
                  "Open-Source Electronic Scholarly Editions of Public Domain Literature"
              div_ [ class_ "col l3 s12" ] $ do
                h5_ [ class_ "white-text" ] $ "Texts"
                ul_ $ do
                  li_ $ a_ [ class_ "white-text", href_ "/texts/#ulysses" ] $ "Ulysses"
                  li_ $ a_ [ class_ "white-text", href_ "/texts/#portrait" ] $ "Portrait"
                  li_ $ a_ [ class_ "white-text", href_ "/texts/#dubliners" ] $ "Dubliners"
                  li_ $ a_ [ class_ "white-text", href_ "/texts/#middlemarch" ] $ "Middlemarch"
              div_ [ class_ "col l3 s12" ] $ do
                h5_ [ class_ "white-text" ] $ "Etc"
                ul_ $ do
                  li_ $ a_ [ class_ "white-text", href_ "https://gitter.im/open-editions/Lobby" ] "Chat with us on Gitter"
                  li_ $ a_ [ class_ "white-text", href_ "https://liberapay.com/JonathanReeve/donate" ] "Donate using Liberapay"
                  li_ $ a_ [ class_ "white-text", href_ "https://github.com/open-editions" ] "Browse our code on GitHub"

            div_ [ class_ "footer-copyright" ] $ div_ [ class_ "container" ] $ do
              "Hand-crafted with love, using "
              a_ [ class_ "orange-text text-lighten-3", href_ "http://materializecss.com" ] $ "Materialize, "
              a_ [ class_ "orange-text text-lighten-3", href_ "https://github.com/srid/rib" ] $ "Rib, "
              a_ [ class_ "orange-text text-lighten-3", href_ "http://haskell.org" ] $ "and Haskell."
              br_ []
              "Licensed under a "
              a_ [ class_ "white-text", href_ "https://creativecommons.org/licenses/by-nc-sa/4.0/" ] "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license."

scriptsTemplate :: Html ()
scriptsTemplate = mapM_ (\src -> with (script_ "") [ src_ src ]) [ "https://code.jquery.com/jquery-2.1.1.min.js"
                                                               , "/static/js/materialize.min.js"
                                                               , "/static/js/init.js"
                                                               ]
