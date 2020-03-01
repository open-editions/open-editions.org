{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad                        (join)
-- import           Control.Monad.Trans.Class            (lift)
import           Control.Applicative                  ((<$>))
-- import           Controllers.Home                     (home, login, post)
import qualified Data.Map as M
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text, concat)
-- import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
-- import           Network.Wai.Middleware.Static        (addBase, noDots,
                                                       -- staticPolicy, (>->))
import           Prelude
import           System.Environment                   (lookupEnv)
-- import           Text.Blaze.Html5                     (Html, pre, toHtml)
import           Text.Read                            (readMaybe)
import           Text.XML
import           Text.XML.Cursor
-- import           Web.Scotty                           (middleware, scotty)

main :: IO ()
main = do
  doc <- Text.XML.readFile def "test-data/header.xml"
  let cursor = fromDocument doc
      filenames = cursor $// laxElement "include" >=> attribute "href"
  print filenames

  -- TODO: load and parse all child files

  Document prologue root epilogue <- Text.XML.readFile def "test-data/3020.xml"
  let root' = transform root
      asText = renderText def (Document prologue root' epilogue)
      rendered = asText -- toHtml asText

  print rendered

transform :: Element -> Element
transform (Element _name attrs children) =
  Element "html" M.empty $ concatMap goNode children

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
    "teiHeader" -> Element "div" (hidden attrs) transformedChildren
    "lg" -> Element "div" lgAttrs transformedChildren
    "l" -> Element "span" M.empty transformedChildren
    "head" -> Element "h1" M.empty transformedChildren
    "lb" -> Element "span" (lbAttrs attrs) (lbChildren attrs)
    "said" -> Element "span" (saidAttrs attrs) (saidChildren attrs children)
    "name" -> spanIt (nameLocalName name) attrs children
    otherwise -> Element name attrs transformedChildren
  where
    transformedChildren = concatMap goNode children
    hidden mattrs = M.insert "class" "hidden" mattrs
    lgAttrs = M.delete "xmlns" $ M.fromList [("class" :: Name, "stanza")]
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
