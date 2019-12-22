---
title: "About Open Editions"
description: "About the project, and this site."
---

The Open Editions project publishes richly annotated open source literary texts. We aim to aggregate the literary knowledge that surrounds a text, codify that knowledge, and integrate it into the text.

[Read the draft article for _James Joyce Quarterly_ where we discuss the project and its preliminary goals.](https://github.com/open-editions/article-jjq/blob/master/open-editions-online.pdf) 

# Introduction

Story here about a detail from Joyce. 

# Guiding Principles

## 1. Openness. 

Distributed version control. 

## 2. Meaningful markup. 

Traditional textual encodings, like HTML, describe the way a text _looks_, rather than what it _is_. Consider italicized words, for example. In early HTML, they would be written `<i>introibo</i>` where `<i>` stands for "italics." Later HTML makes this more semantic, using `<emph>` for emphasized. But a truly semantic markup tells us _why_ a word is italicized. So we use `<foreign xml:lang="la">introibo</foreign>`, which explains that the word is italicized because it is a foreign expression, and also that this is a Latin word (`la` is the two-letter language code for Latin). This kind of semantic markup allows us to be explicit about what we're reading, and how computers should "understand" that meaning. When computers understand a text in this way, it allows humans to manipulate that data, and learn more about the text. This is not meant to replace traditional literary criticism, but take it into the 21st century. 

## 3. Depth before breadth. 

Other projects are interested in gathering as many texts as possible. We are interested in going as deep as possible into each text, as a way of exploring the potential for this semantic approach. 

Thus, we are less interested in discussing ways to import thousands of new documents into our framework. Rather, we're interested in the kinds of things scholars discuss in miscellanies. 


## 4. Standards and correctness. 

It is more important to create a system that is robust and interoperable than to make something pretty. We would rather push clean, reusable data to GitHub than to publish a hastily-assembled website, to have something to show off.

# Technologies

## TEI XML. 

We use the textual markup specification from the [Text Encoding Initiative](https://en.wikipedia.org/wiki/Text_Encoding_Initiative), which has been thinking about ways to encode text since the 80s. While it is true that there are some limitations to XML, we try to mitigate these. 

A common critique of TEI is that there are too many ways to encode the same textual features. A number of projects have tried to correct this. [TEI Lite](https://tei-c.org/guidelines/customization/lite/) and [TEI Simple](https://github.com/TEIC/TEI-Simple) are subsets of TEI intended to disambiguate markup choices. We don't use either of these, but prefer to maintain a list of conventions, documenting how we use certain types of markup.

## Git

Distributed version control is crucial to our work, since it allows us to collaborate widely, using well-tested frameworks. 

## GitHub

GitHub provides a place for us to store our Git repositories, and to communicate with each other via issues. We plot 
