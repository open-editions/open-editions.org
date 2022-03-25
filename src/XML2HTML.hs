{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XML2HTML where

import           Clay                          hiding (transform, type_)
import           Control.Monad                        (join)
import           Control.Applicative                  ((<$>))
import qualified Data.Map.Strict as M
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, concat)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import           Lucid
import           Options.Generic
import           Prelude
import           System.Environment                   (lookupEnv)
import           System.FilePath.Posix                ((-<.>))
import           Text.Read                            (readMaybe)
import           Text.XML
import           Text.XML.Cursor

main :: IO ()
main = do
  -- Extract child files from header.xml
  -- doc <- Text.XML.readFile def "test-data/header.xml"
  -- let cursor = fromDocument doc
  --     filenames = cursor $// laxElement "include" >=> attribute "href"
  -- print filenames

  -- TODO: load and parse all child files

  inFile <- getRecord "XML to HTML converter, for Open Editions TEI files."
  putStrLn $ "Processing: " <> (inFile :: FilePath)
  Document prologue root epilogue <- Text.XML.readFile def (inFile :: FilePath)
  let root' = transform root
      -- Make an empty prologue
      prologue' = Prologue [] Nothing []
      -- Make an empty epilogue
      epilogue' = []
      rendered = Text.XML.renderText def (Document prologue' root' epilogue')
      html = makeHtml (TL.toStrict rendered)
  let htmlFilename = inFile -<.> "html"
  putStrLn $ "Writing to " <> htmlFilename
  Lucid.renderToFile htmlFilename html

transform :: Element -> Element
transform (Element _name attrs children) =
  Element "div" (M.fromList [("class"::Name,"container")]) $ concatMap goNode children

goNode :: Node -> [Node]
goNode (NodeElement e) = [NodeElement $ goElem e]
goNode (NodeContent t) = [NodeContent t]
goNode (NodeComment _) = [] -- hide comments
goNode (NodeInstruction _) = [] -- and hide processing instructions too

-- Convert each source element to its XHTML equivalent
goElem :: Element -> Element
goElem (Element name attrs children) =
  case (nameLocalName name) of
    -- Comment out metadata in header
    "teiHeader" -> Element "div" (hidden attrs) [] -- TODO handle metadata later
    "lg" -> Element "div" lgAttrs transformedChildren
    "l" -> Element "span" (M.fromList [("class"::Name, "line")]) transformedChildren
    "head" -> Element "h1" M.empty transformedChildren
    "lb" -> Element "span" (lbAttrs attrs) (lbChildren attrs)
    "said" -> Element "span" (saidAttrs attrs) (saidChildren attrs children)
    "name" -> spanIt (nameLocalName name) attrs children
    "anchor" -> Element "a" attrs [] -- no children on anchor nodes
    otherwise -> Element name attrs transformedChildren
  where
    transformedChildren = concatMap goNode children
    hidden mattrs = M.insert "class" "hidden" mattrs
    lgAttrs = M.delete "xmlns" $ M.fromList [("class" :: Name, "stanza")]
    -- TODO: convert this line number
    lbAttrs attrs = M.fromList [("class" :: Name, "lineNum")]
    lbChildren attrs = [NodeContent (attrs M.! "n")]
    saidAttrs attrs = setClass "dialogueAttribution"
    saidChildren attrs children = whoTag : transformedChildren
      where
        whoTag = NodeElement $ Element "span" (setClass "attribution") [NodeContent (attrs M.! "who")]

-- processXinclude attrs = do
--   Document prologue root epilogue <- Text.XML.readFile def (attrs M.! "href" :: FilePath)
--   return root

-- Make a Data.Map that looks like [("class":: Name, "lineNum")]
-- So it can be used as an attribute.
mkAttrs :: Name -> Text -> M.Map Name Text
mkAttrs key val = M.fromList [(key :: Name, val)]

-- A wrapper for mkAttrs that can be used to set the HTML class.
setClass :: Text -> M.Map Name Text
setClass val = mkAttrs "class" val

spanIt name attrs children = Element "span" (M.insert "class" name attrs) children

makeHtml :: Text -> Html ()
makeHtml contents = html_ [] $ do
  head_ [] $ do
    style_ [type_ "text/css"] $ Clay.render pageStyle
  body_ [] $ do
    div_ [id_ "lMargin"] ""
    (toHtmlRaw contents)
    div_ [id_ "rMargin"] ""

pageStyle :: Css
pageStyle = do
  "body" ? do
    display flex
    flexDirection row
    justifyContent center
  -- "div.container, #lMargin, #rMargin" ? display flex
  "span.line" ? display block
  "span.lineNum" ? do
    display block
    position relative
    right (px 110)
  "div.stanza" ? (margin (em 1) (em 1) 0 0)
  "img" ? do
    maxWidth (pct 100)
