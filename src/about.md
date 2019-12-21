---
title: "About Open Editions"
---

The Open Editions project publishes richly annotated open source literary texts. We aim to aggregate the literary knowledge that surrounds a text, codify that knowledge, and integrate it into the text.

[Read the draft article for _James Joyce Quarterly_ where we discuss the project and its preliminary goals.](https://github.com/open-editions/article-jjq/blob/master/open-editions-online.pdf) 

# Guiding Principles

1. Openness. 


2. Meaningful markup. 
Traditional textual encodings, like HTML, describe the way a text _looks_, rather than what it _is_. Consider italicized words, for example. In early HTML, they would be written `<i>introibo</i>` where `<i>` stands for "italics." Later HTML makes this more semantic, using `<emph>` for emphasized. But a truly semantic markup tells us _why_ a word is italicized. So we use `<foreign xml:lang="la">introibo</foreign>`, which explains that the word is italicized because it is a foreign expression, and also that this is a Latin word (`la` is the two-letter language code for Latin). This kind of semantic markup allows us to be explicit about what we're reading, and how computers should "understand" that meaning. When computers understand a text in this way, it allows humans to manipulate that data, and learn more about the text. This is not meant to replace traditional literary criticism, but take it into the 21st century. 

3.  


