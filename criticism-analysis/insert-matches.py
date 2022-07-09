#!/usr/bin/env python

# So far, we've been keeping track of match locations using character offsets. But this won't work once we convert the TEI to HTML.
# So we have to add markers in the TEI which we can use later to highlight matches. 
# 
# Let's use [the anchor tag](https://www.tei-c.org/release/doc/tei-p5-doc/en/html/ref-anchor.html). 

import pandas as pd
import re

df = pd.read_csv('fw-matches.csv')

with open("../texts/corpus-joyce-finnegans-wake-tei/finnegans-wake-corrected.xml") as f:
    fw = f.read()

# Melt the data frame such that each match is its own row
melted = {}
for i, row in df.iterrows(): 
    locs = eval(row['Locations in A']) # Is a string
    for loc in locs: 
        data = row.to_dict()
        thisLoc = {"start": loc[0], "end": loc[1]}
        # Make one for each of start, end
        for locType in thisLoc: 
            data = row.to_dict() # Don't really know why I have to do this
            data['loc'] = thisLoc[locType]
            data['locType'] = locType
            index = str(i) + str(loc) + locType
            melted[index] = data


dfMelted = pd.DataFrame(melted).T


tagLocations = [match.span() for match in list(re.finditer('<.*?>', fw))]


def avoidTags(loc, tagLocations=tagLocations):
    """ If a given location is inside a tag, move it until it's outside. """
    for tagStart, tagEnd in tagLocations: 
        if loc >= tagStart and loc <= tagEnd: 
            # This location is inside a tag
            print(loc, ' is inside a tag.')
            return tagEnd
    return loc

dfMelted['locCorrected'] = dfMelted['loc'].apply(avoidTags)

def buildAnchor(df): 
    # Make filenames a little less noisy
    fileNames = " ".join([re.sub('/home/jon/Code/corpus-joyce-finnegans-wake-tei/criticism-analysis/\d/ocr/', '', fn) 
                 for fn in df['Text B'].values])
    indices = " ".join(list(df.index))
    locTypes = df['locType'][0] # We can get away with using [0] since these are only of one type 
    anchor = anchorStart = f'<anchor xml:id="{indices}" corresp="{fileNames}" type="{locTypes}"/>'
    return anchor

newFW = ""
lastLoc = 0
for loc, data in dfMelted.sort_values(by='locCorrected').groupby('locCorrected'): 
    newFW += fw[lastLoc:loc] # Add all text since last time
    #print(f"Now processing location {loc}")
    anchor = buildAnchor(data)
    #print(anchor)
    newFW += anchor # Add anchor
    lastLoc = loc

with open('../finnegans-wake-annotated.xml', 'w') as f:
    f.write(newFW)
