import json
import sys

with open("sortedAnnotations.w3c.json", 'r') as infile:
    o = json.load(infile)
    chunkSize = 200
    for i in xrange(0, len(o), chunkSize):
        with open("chunk" + '_' + str(i//chunkSize) + '.json', 'w') as outfile:
            json.dump(o[i:i+chunkSize], outfile)
