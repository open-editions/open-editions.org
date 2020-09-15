{-# LANGUAGE OverloadedStrings #-}

module Editions where

import Data.Text

type URI = Text
type Markdown = Text --TODO: use actual markdown type

github :: Text
github = "https://github.com/open-editions/"

data Edition = Edition { title :: Text
                       , author :: Text
                       , slug :: Text
                       , features :: [Feature]
                       , repo :: Text
                       , preview :: URI
                       , provenance :: Markdown
                       } deriving Show

-- Make contributors URL
contributors :: Text -> Text
contributors editionRepo = Data.Text.concat [github, editionRepo, "/graphs/contributors"]

-- Make issues URL
issues :: Text -> Text
issues repo = Data.Text.concat [github, repo, "/issues"]

type FeatureDesc = Text

type IssueNo = Maybe Int

data Feature = Feature { desc :: Text
                       , status :: Status
                       , issueNo :: IssueNo
                       } deriving Show

data Status = Todo | Inprogress | Done | Wontfix deriving Show

editions :: [ Edition ]
editions = [ Edition { title = "A Portrait of the Artist as a Young Man"
               , author = "James Joyce"
               , repo = "corpus-joyce-portrait-TEI"
               , slug = "portrait"
               , preview = "https://joyce-portrait.netlify.com"
               , provenance = ""
               , features = [ Feature "Line Numbers" Done (Just 68)
                            , Feature "Dialog Attribution" Inprogress (Just 7)
                            , Feature "Place Names" Done (Just 43)
                            , Feature "Geotagging" Done (Just 43)
                            , Feature "Distinctive Words" Inprogress Nothing
                            , Feature "Cross-References" Inprogress (Just 42)
                            , Feature "Languages" Done Nothing
                            , Feature "Free Indirect Discourse" Todo Nothing
                            , Feature "Criticism" Inprogress (Just 34)
                            , Feature "Zenodo Archive" Done Nothing
                            ]
               }
        , Edition { title = "Ulysses"
                  , author = "James Joyce"
                  , slug = "ulysses"
                  , repo = "corpus-joyce-ulysses-tei"
                  , preview = "https://ulysses-tei.netlify.com/"
                  , provenance = "Based on _Ulysses: A Critical and Synoptic Edition_ (1984 \\[rev. 1986\\]) prepared by Hans Walter Gabler with Wolfhard Steppe and Claus Melchior."
                  , features = [ Feature "Line Numbers" Done Nothing
                               , Feature "Dialog Attribution" Done (Just 7)
                               , Feature "Place Names" Done (Just 49)
                               , Feature "Geotagging" Todo (Just 27)
                               , Feature "Distinctive Words" Inprogress (Just 16)
                               , Feature "Cross-References" Inprogress (Just 42)
                               , Feature "Languages" Done (Just 2)
                               , Feature "Free Indirect Discourse" Todo Nothing
                               , Feature "Criticism" Inprogress Nothing
                               , Feature "Zenodo Archive" Done Nothing
                               ]
                  }
        , Edition { title = "Dubliners"
                  , author = "James Joyce"
                  , slug = "dubliners"
                  , repo = "corpus-joyce-dubliners-tei"
                  , preview = "https://joyce-dubliners.netlify.com/"
                  , provenance = "Based on the Gabler edition." -- TODO: fill this out.
                  , features = [ Feature "Line Numbers" Done Nothing
                               , Feature "Dialog Attribution" Done (Just 7)
                               , Feature "Place Names" Done Nothing
                               , Feature "Geotagging" Done Nothing
                               , Feature "Distinctive Words" Todo Nothing
                               , Feature "Cross-References" Todo Nothing
                               , Feature "Languages" Todo Nothing
                               , Feature "Free Indirect Discourse" Todo Nothing
                               , Feature "Criticism" Todo Nothing
                               , Feature "Zenodo Archive" Todo Nothing
                               ]
                  }
        , Edition { title = "Finnegans Wake"
                  , author = "James Joyce"
                  , slug = "wake"
                  , repo = "corpus-joyce-finnegans-wake-tei"
                  , preview = ""
                  , provenance = "Based on [this TEI edition](https://github.com/timds/finnegans-wake-tei/), and cross-checked with corrections from the Faber 1975 edition."
                  , features = [ Feature "Line Numbers" Done Nothing
                               , Feature "Dialog Attribution" Done (Just 7)
                               , Feature "Place Names" Done Nothing
                               , Feature "Geotagging" Done Nothing
                               , Feature "Distinctive Words" Todo Nothing
                               , Feature "Cross-References" Todo Nothing
                               , Feature "Languages" Todo Nothing
                               , Feature "Free Indirect Discourse" Todo Nothing
                               , Feature "Criticism" Todo Nothing
                               , Feature "Zenodo Archive" Todo Nothing
                               ]
                  }
        , Edition { title = "Middlemarch"
                  , author = "George Eliot"
                  , slug = "middlemarch"
                  , repo = "corpus-eliot-middlemarch-tei"
                  , preview = ""
                  , provenance = "Text based on the Project Gutenberg HTML edition. Dialogue attribution, epigraphs, chapters, and [free indirect discourse](https://en.wikipedia.org/wiki/Free_indirect_speech) are marked up. Project part of Michelle Qiu's senior thesis project at Columbia University."
                  , features = [ Feature "Line Numbers" Done Nothing
                               , Feature "Dialog Attribution" Done (Just 7)
                               , Feature "Place Names" Done Nothing
                               , Feature "Geotagging" Done Nothing
                               , Feature "Distinctive Words" Todo Nothing
                               , Feature "Cross-References" Todo Nothing
                               , Feature "Languages" Todo Nothing
                               , Feature "Free Indirect Discourse" Todo Nothing
                               , Feature "Criticism" Todo Nothing
                               , Feature "Zenodo Archive" Todo Nothing
                               ]
                  }
        ]


