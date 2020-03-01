# Documentation

## Open Editions Specification 

Each edition must: 

 - Be a single git repository, hosted on GitHub.
 - Be named ~corpus-author-shortTitle-tei~, where ~author~ is the author's name, and ~shortTitle~ is a shortened version of the title.
 - Be included in this repository as a submodule (for now).
 - Have a ~header.xml~ in TEI XML, containing all its metadata.
 - Have a ~README.md~ file describing the edition
 - Have a ~.github/workflows/edition.yaml~ file which will validate the XML
 - Have a mirror repo at Zenodo, where it will receive a DOI and have a stable identifier, for posterity.
   - This requires that the edition have a release, and a tag. Use semantic versioning for the tags, e.g. v0.1.0.

The header.xml must: 

 - Be a valid TEI XML file. 
 - Contain a list of xincludes to other files in the repository, if the edition is in more than one file.
 - Contain links to the source repository. 

## Markup Features 

### Dialogue Attribution

Dialogue is marked up with `<said>` and using the `@who` attribute, as in `<said who="Stephen Dedalus">`. 
 
 - For the moment, full names should be used, where known. (Although in the future we hope to shift to XML IDs; see [#19](https://github.com/JonathanReeve/corpus-joyce-ulysses-tei/issues/19). 
 - You can check the list of characters currently used with `make chars`, a command in the [Makefile](https://github.com/JonathanReeve/corpus-joyce-ulysses-tei/blob/master/Makefile). (This requires that you have `ag` (The Silver Searcher) installed, and of course GNU `make`. See [Chris Foster’s comment in issue 19](https://github.com/JonathanReeve/corpus-joyce-ulysses-tei/issues/19#issuecomment-278453253). 

If a character quotes direct speech within her speech, we’re encoding it like this:

```xml
<said who="Stephen Dedalus">―You said,</said> Stephen answered, <said who="Stephen Dedalus"><said who="Buck Mulligan" rend="italics">O, it's only Dedalus whose mother is beastly dead</said>.</said>
```

If direct speech is recalled in interior monologue or (occasionally) represented in the third-person narrative using italics, we’re encoding it like this:

```xml
she was one of those good souls who had always to be told twice <said who="Father Conmee" direct="false" rend="italics">bless you, my child,</said> that they have been absolved, <said who="Father Conmee" direct="false" rend="italics">pray for me</said>.
```

For more on this, see [the discussion in issue #20](https://github.com/JonathanReeve/corpus-joyce-ulysses-tei/issues/20#issuecomment-280171254). 

When it is not clear who is speaking, you can mark up your guesses using the `certainty` tag, like this: 

```xml
<lb n="060004"/><said xml:id="060004-a" who="Cunningham">―Come on, Simon.
<certainty target="#060004-a" match="@who" locus="value" assertedValue="Power" degree="0.5">
    <desc>It's unclear here whether it's Cunningham or Power speaking.</desc>
</certainty>
</said>
```

### Distinctive Words

Distinctive words can be marked up using `<distinct>`, or `<foreign>`, depending on the language. If `<foreign>`, be sure to specify the language using `xml:lang="en"`, where "en" is the two-letter language code.

For lengthy notes on a single word, it might be better to put these in a separate file. See [the word notes in the Ulysses text](https://github.com/open-editions/corpus-joyce-ulysses-tei/blob/master/ulysses-word-notes.xml) for an example. 

Here's one I've generated from the database of [joycewords.com](https://joycewords.com/):

```xml
<note resp="Ronan Crowley" type="analysis" xml:id="151777-bedlock">
<term>bedlock<term>
<analysis>The OED recognises “bedlock” n. as a nonce-word modelled, reasonably enough, after “wedlock.” As it happens, the coinage is not Joyce’s own. He found it in James Huneker’s Painted Veils (1920), which has “whether in wedlock or concubinage—bedlock is the ultimate outcome.”</analysis>
<time>2017-12-06 19:57:28</time>
<bibl>“Born out of bedlock hereditary epilepsy is present, the consequence of unbridled lust.” (U 15.1777–78)</bibl>
</note>
```

Note the format of the xml id, which points to line 151777, and the word "bedlock" appearing in that line.

### Titles of Works

Although the `<title>` tag seems to be used for titles of works in the real world, they might also be applied to fictional works. [The TEI documentation for `<title>`](http://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-title.html) gives information about its attribute `level`, which can be used to distinguish between “levels” of publications such as journals, series, or monographs, but this would not cover all the types present in _Ulysses_, so maybe we could use the attribute `type` along with a few types: 

 - artwork: `<title type="artwork">The Bath of the Nymph</title> over the bed.`
 - book: `smudged pages. <title type="book">Ruby: the Pride of the Ring.</title> Hello.`
 - magazine: `number of <title type="magazine">Photo Bits</title>`
 - newspaper: `The <title type="newspaper">Evening Telegraph</title>`
 - oratorio: `Dead March from <title type="oratorio">Saul</title>.`
 - pantomime: `in the pantomime of <title type="pantomime">Turko the Terrible</title>`
 - play: `Could I go to see <title type="play">Leah</title> tonight, I wonder.`
 - poem: `<title type="poem">Art thou real, my ideal?</title> it was called by Louis J Walsh, Magherafelt`
 - short story: `Our prize titbit: <title type="short story">Matcham’s Masterstroke</title>`
 - song: `<title type="song">Là ci darem</title> with J. C. Doyle, she said, and <title type="song">Love's Old Sweet Song</title>`

Subtitles can be handled, following the TEI suggestions for `@type`, with `<title type=”sub”>`. 

Titles are be rendered as italicized by default, so if a given title isn’t italicized in the text, mark it up with `<title rend=”none”>`. 

### Cross-references

Cross-references show intertextualities between two texts in our collection, or in the same text. For instance, here are a few repeating instances within _Ulysses_:

```xml
    <link xml:id="agenbite" target="u01_telemachus.xml#lb_010481_agenbite u09_scylla.xml#lb_090196_agenbite u09_scylla.xml#lb_090809 u10_wandering_rocks.xml#lb_100875 u10_wandering_rocks.xml#lb_100879"/>
    <note target="agenbite">
        Agenbite of inwit: Middle English for, "again bites the inner wit" (referring to the guilt of conscience). 
        Also the title of a confessional prose work written in a Kentish dialect of Middle English.
        <ref target="https://en.wikipedia.org/wiki/Ayenbite_of_Inwyt"></ref>
    </note>
    <link xml:id="twig-skirt" target="u10_wandering_rocks.xml#lb_100201 u10_wandering_rocks.xml#lb_100440 u14_oxen.xml#lb_141158"/>
    <link xml:id="lemon-soap" target="u05_lotus-eaters.xml#lb_050513 u17_ithaca.xml#lb_170232"/>
```
